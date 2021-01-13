require 'amber/cli/language'
require 'amber/tif/test'

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
      @data.each do |k, v|
        case k
        when 'plan'
          include_this_file v, '--plan'
        when 'suite'
          include_this_file v, '--suite'
        when 'case'
          include_this_file v, '--case'
        when 'file'
          include_this_file v, '--file'
        when 'folder'
          @folder = v
        end
      end
    end

    # ---------------------------------------------------------------------- }}}

    private

    # {{{ A Test Input Factory file need including.

    def include_this_file(name, opt)
       run_amber_command(
        assemble_amber_command(
          assemble_opt_and_files(name, opt)))
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ Assemble options and file names

    def assemble_opt_and_files(name, opt)
      @folder.nil?  ? map_to_files(name, opt)
                    : map_to_nested_files(name, opt)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ map to files

    def map_to_files(name, opt)
      "#{opt}=#{name.map(&:values).join(',')}"
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ map to nested files

    def map_to_nested_files(name, opt)
      opt_and_files = "#{opt}="
      name.each do |k, v|
        opt_and_files << "#{@folder}/"
        opt_and_files << k["name"]
        opt_and_files << "," unless name.last.eql?(k)
      end
      opt_and_files
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ assemble another amber command.

    def assemble_amber_command(opt_and_files)
      cmd = 'amber'
      cmd.concat ' --nodryrun'                           unless @options.dryrun
      cmd.concat ' --log-command'                        if @options.log_command?
      cmd.concat ' --log-requirement'                    if @options.log_requirement?
      cmd.concat ' --simulate'                           if @options.simulate?
      cmd.concat " --writer=#{@options.writer}"          unless @options.writer.nil?
      cmd.concat " --browser=#{@options.browser}"        if @options.has_browser?
      lng = Amber::Language::CODE.key(@options.language) if @options.has_language?
      cmd.concat " --language=#{lng}"                    if @options.has_language?
      cmd.concat " #{opt_and_files}"
      Amber::TestEvidence.record_amber_command(cmd)      if @options.log_command?
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
