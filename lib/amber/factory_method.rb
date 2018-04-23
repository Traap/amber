require 'amber/test_case'
require 'amber/test_plan'
require 'amber/test_step'
require 'amber/test_suite'

require 'amber/ascii_test_case'
require 'amber/ascii_test_plan'
require 'amber/ascii_test_step'
require 'amber/ascii_test_suite'

require 'amber/latex_test_case'
require 'amber/latex_test_plan'
require 'amber/latex_test_step'
require 'amber/latex_test_suite'

require 'amber/environment'
require 'amber/latex_environment'
require 'amber/ascii_environment'

# Amber is the primary module.
module Amber
  # Factory methods used to instantiate Test Plan, Test Suite, Test Cases, Test
  # Steps, and system environment LaTeX or Ascii decorator objects.
  class TestFactory
    def self.get_test_plan(filename, data, options)
      decoratee = TestPlan.new(filename, data, options)
      if options.writer == 'LaTeX'
        LaTeXTestPlan.new(decoratee)
      else
        AsciiTestPlan.new(decoratee)
      end
    end

    def self.get_test_suite(filename, data, options)
      decoratee = TestSuite.new(filename, data, options)
      if options.writer == 'LaTeX'
        LaTeXTestSuite.new(decoratee)
      else
        AsciiTestSuite.new(decoratee)
      end
    end

    def self.get_test_case(filename, data, options)
      decoratee = TestCase.new(filename, data, options)
      if options.writer == 'LaTeX'
        LaTeXTestCase.new(decoratee)
      else
        AsciiTestCase.new(decoratee)
      end
    end

    def self.get_test_step(filename, data, options, step, nbr, workingdir)
      decoratee = TestStep.new(filename, data, options, step, nbr, workingdir)
      if options.writer == 'LaTeX'
        LaTeXTestStep.new(decoratee)
      else
        AsciiTestStep.new(decoratee)
      end
    end

    def self.get_environment(options)
      decoratee = Environment.new
      if options.writer == 'LaTeX'
        LaTeXEnvironment.new(decoratee, options)
      else
        AsciiEnvironment.new(decoratee, options)
      end
    end
  end
end
