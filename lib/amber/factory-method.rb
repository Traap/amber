require 'amber/ascii-test-Case'
require 'amber/ascii-test-Plan'
require 'amber/ascii-test-Step'
require 'amber/ascii-test-Suite'

require 'amber/latex-test-Case'
require 'amber/latex-test-Plan'
require 'amber/latex-test-Step'
require 'amber/latex-test-Suite'

module Amber
  class TestFactory

    def initialize
      # noop
    end

    def get_test_plan(writer, adaptee)
      if writer == "LaTeX"
        LaTeX_TestPlan.new(adaptee)
      else
        Ascii_TestPlan.new(adaptee)
      end
    end

    def get_test_suite(writer, adaptee)
      if writer == "LaTeX"
        LaTeX_TestSuite.new(adaptee)
      else
        Ascii_TestSuite.new(adaptee)
      end
    end

    def get_test_case(writer, adaptee)
      if writer == "LaTeX"
        LaTeX_TestCase.new(adaptee)
      else
        Ascii_TestCase.new(adaptee)
      end
    end

    def get_test_step(writer, adaptee)
      if writer == "LaTeX"
        LaTeX_TestStep.new(adaptee)
      else
        Ascii_TestStep.new(adaptee)
      end
    end

  end # Test Factory
end # Amber 
