require 'amber/test'

module Amber 
  class Ascii_Test < Test
    attr_reader :adaptee

    def initialize(adaptee)
      @adaptee = adaptee
    end

    def setup
      puts "Open [Ascii] xxx-test output file"
    end

    def echo_to_sysout
      @adaptee.echo_to_sysout

      puts "Decorated [Ascii] output ..."
      name = "#{@adaptee.type}: ".rjust(15) << "#{@adaptee.name}"
      puts "#{name}"
      puts "      Purpose: #{@adaptee.purpose}"
      puts "  Requirement: #{@adaptee.requirement}"
    end

    def run_command 
      @adaptee.run_command
    end 

    def teardown
      puts "Close [Ascii] xxx-test output file"
    end

  end # Ascii_Test
end # Amber 
