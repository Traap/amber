require 'amber/test'
require 'amber/outputfiles'

module Amber 
  class Ascii_Test < Test
    attr_reader :adaptee

    def initialize(adaptee)
      @adaptee = adaptee
      @handle = nil
    end

    def setup
      @handle = Amber::TestEvidence.open_log_file(@adaptee.options.filename)
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
      Amber::TestEvidence.close_file(@handle)
    end

  end # Ascii_Test
end # Amber 
