require 'amber/outputfiles'
require 'amber/substitute'
require 'amber/test'

module Amber
  class TestStep < Test
    attr_reader :number, :confirm, :expectation, :command, :evidence, :workingdir

    def initialize(filename, data, options, step, number, workingdir)

      super("Test Step", filename, data, options)

      if step['sudo'] then
        if ["cygwin", "mingw32"].include?(RbConfig::CONFIG['host_os']) then
          sudo = nil
        else
          sudo = "sudo "
        end
      else
        sudo = nil
      end

      @number      = number
      @confirm     = Amber::Substitute.strings(options, step['confirm'])
      @expectation = Amber::Substitute.strings(options, step['expectation'])
      argument     = Amber::Substitute.strings(options, step['argument'])
      @command     = "#{sudo}#{step['command']} #{argument}"
      @evidence    = Amber::Substitute.strings(options, step['evidence'])
      @workingdir  = set_working_dir(step, workingdir)
    end

    def echo_to_sysout; end

    def run_command
      stdout, stderr, status =
        TestEvidence.run_from_temp_directory(@command, @workingdir) if !@options.dryrun
    end

    private
    def set_working_dir(step, workingdir)
      wd = step['workingdir']
      tmp = TestEvidence::assemble_temp_root(@options)

      # This step will used working directory define at steps level.
      if wd.nil? then
        if workingdir.nil? then
          wdir = tmp 
        else
          wdir = tmp + File::SEPARATOR + workingdir
        end

      # This step will nullify the working directory defined at the steps level.
      elsif wd == "nil"
        wdir = tmp 

      # This step will use the working directory it defined.
      else
        wdir = tmp + File::SEPARATOR + wd
      end

      return wdir
    end

  end # TestStep
end # Amber
