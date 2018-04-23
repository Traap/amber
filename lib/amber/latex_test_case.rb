require 'amber/latex_test'
require 'amber/latex_utility'
require 'fileutils'

module Amber
  # Decorate Test Case output with LaTeX text.
  class LaTeX_TestCase < LaTeX_Test
    def initialize(decoratee)
      macro = LaTeX_Utility.get_case_macro(decoratee)
      super(decoratee, macro)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call('paragraph')
    end
  end
end
