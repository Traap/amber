require 'amber/test-case'
require 'amber/test-plan'
require 'amber/test-step'
require 'amber/test-suite'

require 'amber/ascii-test-case'
require 'amber/ascii-test-plan'
require 'amber/ascii-test-step'
require 'amber/ascii-test-suite'

require 'amber/latex-test-case'
require 'amber/latex-test-plan'
require 'amber/latex-test-step'
require 'amber/latex-test-suite'

require 'amber/environment'
require 'amber/latex-environment'
require 'amber/ascii-environment'

module Amber
  class TestFactory

    def TestFactory.get_test_plan(filename, data, options)
      decoratee = TestPlan.new(filename, data, options)
      if options.writer == "LaTeX"
        LaTeX_TestPlan.new(decoratee)
      else
        Ascii_TestPlan.new(decoratee)
      end
    end

    def TestFactory.get_test_suite(filename, data, options)
      decoratee = TestSuite.new(filename, data, options)
      if options.writer == "LaTeX"
        LaTeX_TestSuite.new(decoratee)
      else
        Ascii_TestSuite.new(decoratee)
      end
    end

    def TestFactory.get_test_case(filename, data, options)
      decoratee = TestCase.new(filename, data, options)
      if options.writer == "LaTeX"
        LaTeX_TestCase.new(decoratee)
      else
        Ascii_TestCase.new(decoratee)
      end
    end

    def TestFactory.get_test_step(filename, data, options, step, nbr, workingdir)
      decoratee = TestStep.new(filename, data, options, step, nbr, workingdir)
      if options.writer == "LaTeX"
        LaTeX_TestStep.new(decoratee)
      else
        Ascii_TestStep.new(decoratee)
      end
    end

    def TestFactory.get_equipment(options)
      decoratee = Environment.new()
      if options.writer == "LaTeX"
        LaTeX_Environment.new(decoratee, options)
      else
        Ascii_Environment.new(decoratee, options)
      end
    end
  end # Test Factory
end # Amber 
