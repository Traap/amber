require 'amber/latex-test'

module Amber 
  class LaTeX_TestPlan < LaTeX_Test 

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

  end # LaTeX_TestPlan
end # Amber 
