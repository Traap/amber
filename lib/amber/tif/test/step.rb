# frozen_string_literal: true

require 'amber/tif/test'
require 'amber/tof/testevidence'
require 'amber/utility/substitute'

module Amber
  # A test step executes a command.
  class TestStep < Amber::Test
    attr_reader :number, :confirm, :expectation, :command, :evidence, :workingdir

    # {{{ Initialize TestStep

    def initialize(filename, data, options, step, number, workingdir)
      super('Test Step', filename, data, options)

      define_expectation(options, step, number)
      define_command(options, step, workingdir)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ define_expectation

    def define_expectation(options, step, number)
      @number      = number
      @confirm     = Amber::Substitute.strings(@filename, options, step['confirm'])
      @expectation = Amber::Substitute.strings(@filename, options, step['expectation'])
      @evidence    = Amber::Substitute.strings(@filename, options, step['evidence'])
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ define_command

    def define_command(options, step, workingdir)
      @workingdir  = set_working_dir(step, workingdir)

      command = Amber::Substitute.expand_path(
        Amber::Substitute.strings(@filename, options, step['command'])
      )

      argument = Amber::Substitute.expand_path(
        Amber::Substitute.strings(@filename, options, step['argument'])
      )

      echo = @options.simulate? ? 'echo ' : nil
      sudo = use_sudo(step)

      @command = "#{echo}#{sudo}#{command} #{argument}"
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ use_sudo

    def use_sudo(step)
      return unless step['sudo']

      if %w[cygwin mingw32].include?(RbConfig::CONFIG['host_os'])
        nil
      else
        'sudo '
      end
    end

    # ---------------------------------------------------------------------- }}}

    def echo_to_sysout; end

    # {{{ Run a command

    def run_command
      _o, _e, _s = TestEvidence.run_from_temp_directory(@command, @workingdir) if @options.run?
    end

    # ---------------------------------------------------------------------- }}}

    private

    # {{{ Set the working diectory.

    # rubocop:disable Metrics.MethodLength
    def set_working_dir(step, workingdir)
      wd = step['workingdir']
      wd = Amber::Substitute.expand_path(wd) unless wd.nil?

      tmp = TestEvidence.assemble_temp_root(@options)

      # This step will use the working directory define at test case level.
      wdir = if wd.nil?
               if workingdir.nil?
                 tmp
               else
                 tmp + File::SEPARATOR + workingdir
               end

             # This step will nullify the working directory defined at the steps
             # level.
             elsif wd == 'nil'
               tmp

             # This step will use the working directory defined at the step
             # level.
             else
               tmp + File::SEPARATOR + wd
             end

      wdir.strip
    end
    # rubocop:enable Metrics.MethodLength

    # ---------------------------------------------------------------------- }}}
  end
end
