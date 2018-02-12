require 'amber/latex-test'

module Amber 
  class LaTeX_TestPlan < LaTeX_Test 

    def initialize(adaptee)
      super(adaptee)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call 
    end

  end # LaTeX_TestPlan
end # Amber 
