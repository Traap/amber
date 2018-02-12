module Amber 
  class Ascii_TestSuite < Ascii_Test 

    def initialize(adaptee)
      super(adaptee)
    end

    def echo_to_sysout
      @adaptee.echo_to_sysout
      method(:echo_to_sysout).super_method.call 
    end

    def run_command 
      @adaptee.run_command
    end 

  end # Ascii_TestSuite
end # Amber 
