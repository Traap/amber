require 'amber/latex-test'
require 'amber/latex-utility'
require 'fileutils'

module Amber
  class LaTeX_TestSuite < LaTeX_Test

    def initialize(decoratee)
      macro =  LaTeX_Utility.get_suite_macro(decoratee)
      super(decoratee, macro)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call("subsubsection")
    end

  end # LaTeX_TestSuite
end # Amber
