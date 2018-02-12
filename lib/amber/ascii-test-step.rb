require 'amber/ascii-test'

module Amber 
  class ShellError < StandardError; end

  class Ascii_TestStep < Ascii_Test

    def initialize(adaptee)
      super(adaptee)
    end

    def echo_to_sysout
      puts "         Step: #{@adaptee.number}"
      puts "      Confirm: #{@adaptee.confirm}"
      puts "  Expectation: #{@adaptee.expectation}"
      puts "      Command: #{@adaptee.command}"
      @adaptee.echo_to_sysout
    end

    def run_command 
      begin
        stdout, stderr, status = @adaptee.run_command 
        result = status ? "PASS" : "FAIL"
        puts "  Test Result: #{result}"
        puts "     Evidence: #{@adaptee.evidence}"
        puts "#{stdout}"
        puts ""
      rescue ShellError
        puts "#{stderr}"
        abort "System command failed: #{status}"
      end
    end 

  end # Ascii_TestStep 
end # Amber 
