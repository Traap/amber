# frozen_string_literal: true

# {{{ Required files.

require 'amber/tof/writers/ascii/test'

# -------------------------------------------------------------------------- }}}
module Amber
  # {{{ Claa ShellError

  class ShellError < StandardError; end

  # ------------------------------------------------------------------------ }}}
  # {{{ Decorate test step output with Ascii text. }}}
  class AsciiTestStep < Amber::AsciiTest
    # {{{ initialize

    def initialize(decoratee)
      super(decoratee)
      @test_result = 'FAIL'
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_to_sysout

    def echo_to_sysout
      @handle.write "         Step: #{@decoratee.number}\n"
      @handle.write "      Confirm: #{@decoratee.confirm}\n"
      @handle.write "  Expectation: #{@decoratee.expectation}\n"
      @handle.write "      Command: #{@decoratee.command}\n"
      @decoratee.echo_to_sysout
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ run_command

    def run_command
      time_start = Time.new
      stdout, stderr, status = @decoratee.run_command
      time_end = Time.new
      @test_result, output = check_status(stdout, stderr, status)
      @handle.write  "  Execution start: #{time_start.strftime('%b %d, %Y %T.%6N')}\n"
      @handle.write  "  Execution end: #{time_end.strftime('%b %d, %Y %T.%6N')}\n"
      @handle.write  "  Test Result: #{@test_result}\n"
      @handle.write  "     Evidence: #{@decoratee.evidence}\n"
      @handle.write  "#{output}\n"
      @handle.flush
    rescue ShellError
      msg = "System command failed: #{status}"
      @handle.write "#{stderr}\n#{msg}\n"
      @handle.flush
      puts "#{msg}\n" if @options.verbose
      abort msg
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ check_status

    def check_status(stdout, stderr, status)
      if status.success?
        @test_result = 'PASS'
        output = stdout
      else
        @test_result = 'FAIL'
        output = stderr
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
