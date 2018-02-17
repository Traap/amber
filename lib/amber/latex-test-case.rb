require 'amber/latex-test'

module Amber
  class LaTeX_TestCase < LaTeX_Test 

    def initialize(decoratee)
      super(decoratee)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call 
    end

  end # LaTeX_TestCase
end # Amber
