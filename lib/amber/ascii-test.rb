require 'amber/test'
require 'amber/outputfiles'

module Amber 
  class Ascii_Test < Test
    attr_reader :decoratee, :macro

    def initialize(decoratee)
      if decoratee.type == "Test Step" then
        @macro = nil
      else
        @macro = decoratee.type + ": " + 
                 File.basename(decoratee.filename, ".*") + "\n"
      end
      @decoratee   = decoratee
      @type        = decoratee.type
      @filename    = decoratee.filename
      @data        = decoratee.data
      @name        = decoratee.name
      @purpose     = decoratee.purpose
      @requirement = decoratee.requirement
      @options     = decoratee.options
      @command     = decoratee.command
      @handle      = nil
    end

    def setup
      @handle = Amber::TestEvidence.open_log_file(@decoratee.filename,
                                                  @decoratee.options)
    end

    def echo_to_sysout
      @decoratee.echo_to_sysout

      name = "#{@decoratee.type}: ".rjust(15) << "#{@decoratee.name}"
      @handle.write "#{name}\n"
      @handle.write "      Purpose: #{@decoratee.purpose}\n"
      @handle.write "  Requirement: #{@decoratee.requirement}\n\n" if !@decoratee.requirement.nil?
      @handle.flush
    end

    def run_command 
      @decoratee.run_command if !@options.dryrun
    end 

    def teardown
      Amber::TestEvidence.record_test_name(@macro, @decoratee.options) if !@macro.nil?
      Amber::TestEvidence.close_file(@handle)
      @handle = nil
    end

  end # Ascii_Test
end # Amber 
