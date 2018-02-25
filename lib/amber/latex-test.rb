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
      @handle = Amber::TestEvidence.open_log_file(@decoratee.filename,
                                                  @decoratee.options)
    end

    def echo_to_sysout
      @decoratee.echo_to_sysout
      
      name = "#{@decoratee.type}: #{@decoratee.name}"
      @handle.write "\\subsection{#{name}}\n"
      @handle.write "\\begin{description}[align=right,leftmargin=*,labelindent=3cm]\n"
      @handle.write "\\item[Purpose:] #{@decoratee.purpose}\n"
      @handle.write "\\item[Requirement:] #{@decoratee.requirement}\n"
      @handle.write "\\end{description}\n"
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
