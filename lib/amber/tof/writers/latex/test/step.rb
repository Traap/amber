# frozen_string_literal: true

# {{{ Required files.

require 'amber/tof/writers/latex/test'
require 'amber/tof/writers/latex/string_to_latex'
require 'time'

# -------------------------------------------------------------------------- }}}
module Amber
  # {{{ Class ShellError

  class ShellError < StandardError; end

  # ------------------------------------------------------------------------ }}}
  # Decorate test step output with LaTeX text.
  class LaTeXTestStep < Amber::LaTeXTest
    # {{{ initilize

    def initialize(decoratee)
      super(decoratee, nil)
      @test_result = 'FAIL'
      @time_stamp = Time.new
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ setup

    def setup
      @handle = TestEvidence.open_file(
        Amber::TestEvidence.get_test_case_log(
          @decoratee.filename,
          @decoratee.number,
          @decoratee.options
        )
      )
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_to_sysout

    def echo_to_sysout
      @handle.write "\\begin{description}[align=right,leftmargin=3.2cm,labelindent=3.0cm]\n"
      @handle.write "\\item[Step:] #{@decoratee.number}\n"
      @handle.write "\\item[Confirm:] #{toLaTeX(@decoratee.confirm)}\n"
      @handle.write "\\item[Expectation:] #{toLaTeX(@decoratee.expectation)}\n"
      @handle.write "\\item[Command:] #{toLaTeX(@decoratee.command)}\n"
      @decoratee.echo_to_sysout
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ run_command

    def run_command
      time_start = Time.new
      stdout, stderr, status = @decoratee.run_command
      @test_result, output = check_status(stdout, stderr, status)
      time_end = Time.new
      record_evidence(time_start, time_end, output)
    rescue ShellError
      msg = "System command failed: #{status}"
      @handle.write "#{stderr}\n#{msg}\n"
      @handle.flush
      abort msg
    end

    def record_evidence(time_start, time_end, output)
      @handle.write "\\item[Execution start:] #{toLaTeX(time_start.strftime('%b %d, %Y %T.%6N'))}\n"
      @handle.write "\\item[Execution end:] #{toLaTeX(time_end.strftime('%b %d, %Y %T.%6N'))}\n"
      @handle.write "\\item[Test Result:] #{toLaTeX(@test_result)}\n"
      @handle.write "\\item[Evidence:] #{toLaTeX(@decoratee.evidence)}\n"
      @handle.write "\\end{description}\n"
      @handle.write "\\begin{lstlisting}[numbers=left]\n"
      @handle.write "#{output}\n"
      @handle.write "\\end{lstlisting}\n"
      @handle.flush
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ check_status

    def check_status(stdout, stderr, status)
      if status.success?
        @test_result = 'PASS'
        output = stdout
      else
        @test_result = 'FAIL'
        output = "#{stderr}\n#{stdout}\n"
      end
      [@test_result, output]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ teardown

    def teardown
      Amber::TestEvidence.record_test_case_status(
        @decoratee.filename, @decoratee.number, @test_result, @decoratee.options
      )
      method(:teardown).super_method.call
    end

    # ---------------------------------------------------------------------- }}}
  end
end
