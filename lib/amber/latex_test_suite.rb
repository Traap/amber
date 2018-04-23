require 'amber/latex_test'
require 'amber/latex_utility'
require 'fileutils'

module Amber
  # Decorate test suite output with LaTeX text.
  class LaTeXTestSuite < LaTeXTest
    def initialize(decoratee)
      macro = LaTeXUtility.get_suite_macro(decoratee)
      super(decoratee, macro)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call('subsubsection')
    end
  end
end
