require 'amber/test'
require 'amber/outputfiles'

module Amber 
  class LaTeX_Test < Test
    attr_reader :decoratee

    def initialize(decoratee)
      @decoratee = decoratee
      @handle = nil
    end

    def setup
      @handle = Amber::TestEvidence.open_log_file(@decoratee.filename)
    end

    def echo_to_sysout
      @decoratee.echo_to_sysout
      
      name = "#{@decoratee.type}: ".rjust(15) << "#{@decoratee.name}"
      @handle.write "#{name}\n"
      @handle.write "      Purpose: #{@decoratee.purpose}\n"
      @handle.write "  Requirement: #{@decoratee.requirement}\n\n"
      @handle.flush
    end

    def run_command 
      @decoratee.run_command
    end 

    def teardown
      Amber::TestEvidence.close_file(@handle)
      @handle = nil
    end

  end # LaTeX_Test
end # Amber 
