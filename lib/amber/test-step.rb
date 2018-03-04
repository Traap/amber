require 'amber/test'
require 'amber/outputfiles'

module Amber
  class TestStep < Test
    attr_reader :number, :confirm, :expectation, :command, :evidence, :workingdir

    def initialize(filename, data, options, step, number, workingdir)

      super("Test Step", filename, data, options)

      if step['sudo'] then
        sudo = RbConfig::CONFIG['host_os'] == "cygwin" ? nil : "sudo "
      else
        sudo = nil
      end

      @number = number
      @confirm = step['confirm']
      @expectation = step['expectation']
      @command = "#{sudo}#{step['command']} #{step['argument']}"
      @evidence = step['evidence']
      @workingdir = set_working_dir(step, workingdir)
    end

    def echo_to_sysout; end

    def run_command
      stdout, stderr, status =
        TestEvidence.run_from_temp_directory(@command, @workingdir) if !@options.dryrun
    end

    private
    def set_working_dir(step, workingdir)
      wd = step['workingdir']

      # This step will used working directory define at steps level.
      if wd.nil? then
        if workingdir.nil? then
          wdir = TestEvidence::Temp_Dir
        else 
          wdir = TestEvidence::Temp_Dir + File::SEPARATOR + workingdir 
        end

      # This step will nullify the working directory defined at the steps level.
      elsif wd == "nil"
        wdir = TestEvidence::Temp_Dir

      # This step will use the working directory it defined.
      else
        wdir = TestEvidence::Temp_Dir + File::SEPARATOR + wd 
      end

      return wdir
    end

  end # TestStep
end # Amber
