require 'amber/latex-test'
require 'fileutils'

module Amber
  class LaTeX_TestCase < LaTeX_Test 

    def initialize(decoratee)
      macro = "\\tco{" + File.basename(decoratee.filename, ".*") + "}\n" 
      super(decoratee, macro)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call 
    end

  end # LaTeX_TestCase
end # Amber
