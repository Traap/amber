# frozen_string_literal: true

# {{{ Required files.

require 'amber/cli/language'
require 'amber/tif/test'
require 'shellwords'

# -------------------------------------------------------------------------- }}}
module Amber
  # A YAML directive to include another file.
  class Include < Amber::Test
    # {{{ Initialize Include File Object

    def initialize(data, options)
      super(nil, 'Include', data, options)
    end

    # ---------------------------------------------------------------------- }}}

    def echo_to_sysout; end

    # {{{{ run a command

    def run_command
      @data.each do |key, value|
        if %w[plan suite case file].include? key
          include_this_file value, "--#{key}"
        else
          @folder = value
        end
      end
    end

    # ---------------------------------------------------------------------- }}}

    private

    # {{{ A Test Input Factory file is being included.

    def include_this_file(name, opt)
      run_amber_command(
        assemble_amber_command(
          assemble_opt_and_files(name, opt)
        )
      )
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Assemble options and file names

    def assemble_opt_and_files(name, opt)
      if @folder.nil?
        map_to_files(name, opt)
      else
        map_to_nested_files(name, opt)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ map to files

    def map_to_files(name, opt)
      "#{opt}=#{include_names(name).join(',')}"
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ map to nested files

    def map_to_nested_files(name, opt)
      nested_names = include_names(name).map { |entry| "#{@folder}/#{entry}" }
      "#{opt}=#{nested_names.join(',')}"
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ include names

    def include_names(name)
      Array(name).map { |entry| include_name(entry) }
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ include name

    def include_name(entry)
      case entry
      when Hash
        entry.fetch('name')
      else
        entry.to_s
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ assemble another amber command.

    def assemble_amber_command(opt_and_files)
      cmd = ''.dup
      cmd << amber_command.shellescape
      cmd << log_commands
      cmd << simulate_run
      cmd << tof_writer
      cmd << browser_and_language
      cmd << " #{opt_and_files}"
      Amber::TestEvidence.record_amber_command(cmd) if @options.log_command?
      cmd
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ amber command

    def amber_command
      return 'amber' if ENV['AMBERPATH'].to_s.empty?

      File.join(ENV.fetch('AMBERPATH'), 'bin', 'amber')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ log_commands

    def log_commands
      cmd = ''.dup
      cmd << ' --nodryrun'        unless @options.dryrun?
      cmd << ' --log-command'     if @options.log_command?
      cmd << ' --log-requirement' if @options.log_requirement?
      cmd
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ simulate_run

    def simulate_run
      cmd = ''.dup
      cmd << ' --simulate' if @options.simulate?
      cmd
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ tof_writer

    def tof_writer
      cmd = ''.dup
      cmd << " --writer=#{@options.writer}" unless @options.writer.nil?
      cmd
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ browser_and_language

    def browser_and_language
      cmd = ''.dup
      lng = Amber::Language::CODE.key(@options.language) if @options.language?

      cmd << " --browser=#{@options.browser}"            if @options.browser?
      cmd << " --language=#{lng}"                        if @options.language?
      cmd
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ run an amber command.

    def run_amber_command(cmd)
      @command = cmd
      method(:run_command).super_method.call
    end

    # ---------------------------------------------------------------------- }}}
  end
end
