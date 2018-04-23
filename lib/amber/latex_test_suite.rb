require 'amber/latex_test'
require 'amber/latex_utility'
require 'fileutils'

module Amber
  # Decorate test suite output with LaTeX text.
  class LaTeX_TestSuite < LaTeX_Test
    def initialize(decoratee)
      macro = LaTeX_Utility.get_suite_macro(decoratee)
      super(decoratee, macro)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call('subsubsection')
    end
  end
end
