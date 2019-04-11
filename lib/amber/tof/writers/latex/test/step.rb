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

    def echo_to_sysout
      @handle.write "\\begin{lstlisting}[numbers=left]\n"
      @handle.write "       Step: #{@decoratee.number}\n"
      @handle.write "    Confirm: #{@decoratee.confirm}\n"
      @handle.write "Expectation: #{@decoratee.expectation}\n"
      @handle.write "    Command: #{@decoratee.command}\n"
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
        @handle.write "Test Result: #{@test_result}\n"
        @handle.write "   Evidence: #{@decoratee.evidence}\n"
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
      Amber::TestEvidence.record_final_test_result(@decoratee.filename,
                                                   @decoratee.number,
                                                   @test_result,
                                                   @decoratee.options)
      method(:teardown).super_method.call
    end
  end
end
