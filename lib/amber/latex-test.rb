require 'amber/test'

module Amber 
  class LaTeX_Test < Test
    attr_reader :adaptee

    def initialize(adaptee)
      @adaptee = adaptee
    end

    def echo_to_sysout
      @adaptee.echo_to_sysout

      name = "#{@adaptee.type}: ".rjust(15) << "#{@adaptee.name}"
      puts "#{name}"
      puts "      Purpose: #{@adaptee.purpose}"
      puts "  Requirement: #{@adaptee.requirement}"
    end

    def run_command 
      @adaptee.run_command
    end 

  end # LaTeX_Test
end # Amber 
