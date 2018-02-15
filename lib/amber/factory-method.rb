require 'amber/test-Case'
require 'amber/test-Plan'
require 'amber/test-Step'
require 'amber/test-Suite'

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

    def TestFactory.get_test_plan(data, options)
      adaptee = TestPlan.new(data, options)
      if options.writer == "LaTeX"
        LaTeX_TestPlan.new(adaptee)
      else
        Ascii_TestPlan.new(adaptee)
      end
    end

    def TestFactory.get_test_suite(data, options)
      adaptee = TestSuite.new(data, options)
      if options.writer == "LaTeX"
        LaTeX_TestSuite.new(adaptee)
      else
        Ascii_TestSuite.new(adaptee)
      end
    end

    def TestFactory.get_test_case(data, options)
      adaptee = TestCase.new(data, options)
      if options.writer == "LaTeX"
        LaTeX_TestCase.new(adaptee)
      else
        Ascii_TestCase.new(adaptee)
      end
    end

    def TestFactory.get_test_step(step, nbr, options)
      adaptee = TestStep.new(step, nbr)
      if options.writer == "LaTeX"
        LaTeX_TestStep.new(adaptee)
      else
        Ascii_TestStep.new(adaptee)
      end
    end

  end # Test Factory
end # Amber 
