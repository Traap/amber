# frozen_string_literal: true

require 'amber/tof/writers/latex/test'

module Amber
  # Shell
  class ShellError < StandardError; end

  # Decorate test step output with LaTeX text.
  class LaTeXTestStep < Amber::LaTeXTest
    def initialize(decoratee)
      super(decoratee, nil)
      @test_result = 'FAIL'
    end

    def setup
      @handle = TestEvidence.open_file(
        Amber::TestEvidence.get_test_case_log(
          @decoratee.filename, 
          @decoratee.number,
          @decoratee.options
        )
      )
    end

    def echo_to_sysout
      @handle.write "\\begin{description}[align=right,leftmargin=3.2cm,labelindent=3.0cm]\n"
      @handle.write "\\item[Step:] #{toLaTeX(@decoratee.number)}\n"
      @handle.write "\\item[Confirm:] #{toLaTeX(@decoratee.confirm)}\n"
      @handle.write "\\item[Expectation:] #{toLaTeX(@decoratee.expectation)}\n"
      @handle.write "\\item[Command:] #{toLaTex(@decoratee.command)}\n"
      @decoratee.echo_to_sysout
    end

    def run_command
      begin
        stdout, stderr, status = @decoratee.run_command
        if status.success?
          @test_result = 'PASS'
          output = stdout
        else
          @test_result = 'FAIL'
          output = "#{stderr}\n#{stdout}\n"
        end
        @handle.write "\\item[Test Result:] #{toLaTeX(@test_result)}\n"
        @handle.write "\\item[Evidence:] #{toLaTeX(@decoratee.evidence)}\n"
        @handle.write "\\end{description}\n"

        @handle.write "\\begin{lstlisting}[numbers=left]\n"
        @handle.write "#{output}\n"
        @handle.write "\\end{lstlisting}\n"
        @handle.flush
      rescue ShellError
        msg = "System command failed: #{status}"
        @handle.write "#{stderr}\n#{msg}\n"
        @handle.flush
        abort msg
      end
    end

    def teardown
      Amber::TestEvidence.record_test_case_status(
        @decoratee.filename, @decoratee.number, @test_result, @decoratee.options
      )
      method(:teardown).super_method.call
    end
  end
end
