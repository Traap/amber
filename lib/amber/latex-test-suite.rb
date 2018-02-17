require 'amber/latex-test'

module Amber
  class LaTeX_TestSuite < LaTeX_Test 

    def initialize(decoratee)
      super(decoratee)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call 
    end

  end # LaTeX_TestSuite
end # Amber 
