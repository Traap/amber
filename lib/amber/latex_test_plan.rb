require 'amber/latex_test'
require 'amber/latex_utility'
require 'fileutils'

module Amber
  # Decorate test plan output with LaTeX text.
  class LaTeXTestPlan < LaTeXTest
    def initialize(decoratee)
      macro = LaTeXUtility.get_plan_macro(decoratee)
      super(decoratee, macro)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call('subsection')
    end
  end
end
