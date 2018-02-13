require 'amber/test'

module Amber 
  class LaTeX_Test < Test
    attr_reader :adaptee

    def initialize(adaptee)
      @adaptee = adaptee
    end

    def setup
      puts "Open [LaTeX] xxx-test output file"
    end

    def echo_to_sysout
      @adaptee.echo_to_sysout
      
      puts "Decorated [LaTeX] output ..."
      name = "#{@adaptee.type}: ".rjust(15) << "#{@adaptee.name}"
      puts "#{name}"
      puts "      Purpose: #{@adaptee.purpose}"
      puts "  Requirement: #{@adaptee.requirement}"
    end

    def run_command 
      @adaptee.run_command
    end 

    def teardown
      puts "Close [LaTeX] xxx-test output file"
    end

  end # LaTeX_Test
end # Amber 
