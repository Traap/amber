require 'amber/latex-test'
require 'fileutils'

module Amber
  class LaTeX_TestPlan < LaTeX_Test

    def initialize(decoratee)
      macro = "\\tpo{" + File.basename(decoratee.filename, ".*") + "}\n"
      super(decoratee, macro)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call
    end

  end # LaTeX_TestPlan
end # Amber
