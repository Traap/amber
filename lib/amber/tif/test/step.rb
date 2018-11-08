# frozen_string_literal: true

require 'amber/tif/test'
require 'amber/tof/testevidence'
require 'amber/utility/substitute'

module Amber
  class TestStep < Amber::Test
    attr_reader :number, :confirm, :expectation, :command, :evidence, :workingdir

    def initialize(filename, data, options, step, number, workingdir)
      super('Test Step', filename, data, options)

      sudo = if step['sudo']
               if %w[cygwin mingw32].include?(RbConfig::CONFIG['host_os'])
                 nil
               else
                 'sudo '
               end
             end

      @number      = number
      @confirm     = Amber::Substitute.strings(@filename, options, step['confirm'])
      @expectation = Amber::Substitute.strings(@filename, options, step['expectation'])

      command      = Amber::Substitute.expand_path(
        Amber::Substitute.strings(@filename, options, step['command'])
      )

      argument = Amber::Substitute.expand_path(
        Amber::Substitute.strings(@filename, options, step['argument'])
      )

      echo = @options.simulate ? 'echo ' : nil
      @command     = "#{echo}#{sudo}#{command} #{argument}"

      @evidence    = Amber::Substitute.strings(@filename, options, step['evidence'])
      @workingdir  = set_working_dir(step, workingdir)
    end

    def echo_to_sysout; end

    def run_command
      if @options.okay_to_run?
        stdout, stderr, status = TestEvidence.run_from_temp_directory(
          @command, @workingdir
        )
      end
    end

    private

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

             # This step will nullify the working directory defined at the steps level.
             elsif wd == 'nil'
               tmp

             # This step will use the working directory defined at the step level.
             else
               tmp + File::SEPARATOR + wd
             end

      wdir.strip
    end
  end
end
