require 'amber/latex-test'
require 'amber/latex-utility'
require 'fileutils'

module Amber
  class LaTeX_TestCase < LaTeX_Test 

    def initialize(decoratee)
      macro = LaTeX_Utility.get_case_macro(decoratee)
      super(decoratee, macro)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call 
    end

  end # LaTeX_TestCase
end # Amber
