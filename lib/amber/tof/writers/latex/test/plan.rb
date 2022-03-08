# frozen_string_literal: true

# {{{ Required files

require 'amber/tof/writers/latex/test'
require 'amber/tof/writers/latex/utility'
require 'fileutils'

# -------------------------------------------------------------------------- }}}
module Amber
  # Decorate test plan output with LaTeX text.
  class LaTeXTestPlan < Amber::LaTeXTest
    # {{{ initialize

    def initialize(decoratee)
      macro = LaTeXUtility.get_plan_macro(decoratee)
      super(decoratee, macro)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_to_sysout

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call('subsection')
    end

    # ---------------------------------------------------------------------- }}}
  end
end
