# frozen_string_literal: true

require 'amber/tof/writers/latex/test'
require 'amber/tof/writers/latex/utility'
require 'fileutils'

module Amber
  # Decorate test suite output with LaTeX text.
  class LaTeXTestSuite < Amber::LaTeXTest
    def initialize(decoratee)
      macro = LaTeXUtility.get_suite_macro(decoratee)
      super(decoratee, macro)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call('subsubsection')
    end
  end
end
