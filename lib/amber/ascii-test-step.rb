require 'amber/ascii-test'

module Amber 
  class ShellError < StandardError; end

  class Ascii_TestStep < Ascii_Test

    def initialize(decoratee)
      super(decoratee)
      @test_result = "FAIL"
    end

    def echo_to_sysout
      @handle.write "         Step: #{@decoratee.number}\n"
      @handle.write "      Confirm: #{@decoratee.confirm}\n"
      @handle.write "  Expectation: #{@decoratee.expectation}\n"
      @handle.write "      Command: #{@decoratee.command}\n"
      @decoratee.echo_to_sysout
    end

    def run_command 
      begin
        stdout, stderr, status = @decoratee.run_command 
        @test_result = status ? "PASS" : "FAIL"
        @handle.write  "  Test Result: #{@test_result}\n"
        @handle.write  "     Evidence: #{@decoratee.evidence}\n"
        @handle.write  "#{stdout}\n"
        @handle.flush
      rescue ShellError
        msg = "System command failed: #{status}"
        @handle.write "#{stderr}\n#{msg}\n"
        @handle.flush
        puts "#{msg}\n" if @options.verbose
        abort msg
      end

      Amber::TestEvidence.record_final_test_result(@decoratee.filename, 
                                                   @decoratee.number, 
                                                   @test_result,
                                                   @decoratee.options)
    end 

  end # Ascii_TestStep 
end # Amber 
