require 'amber/latex-test'

module Amber
  class ShellError < StandardError; end

  class LaTeX_TestStep < LaTeX_Test

    def initialize(adaptee)
      super(adaptee)
    end

    def echo_to_sysout(nbr)
      puts "         Step: #{nbr}"
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
        puts "     Evidence: #{@evidence}"
        puts "#{stdout}"
        puts ""
      rescue ShellError
        puts "#{stderr}"
        abort "System command failed: #{status}"
      end
      @adaptee.run_command
    end

  end # LaTeX_TestStep
end # Amber
