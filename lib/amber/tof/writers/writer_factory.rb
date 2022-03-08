# frozen_string_literal: true

# {{{ Required files.

require 'amber/tif/test/case'
require 'amber/tif/test/plan'
require 'amber/tif/test/step'
require 'amber/tif/test/suite'

require 'amber/tof/writers/ascii/test/case'
require 'amber/tof/writers/ascii/test/plan'
require 'amber/tof/writers/ascii/test/step'
require 'amber/tof/writers/ascii/test/suite'

require 'amber/tof/writers/latex/test/case'
require 'amber/tof/writers/latex/test/plan'
require 'amber/tof/writers/latex/test/step'
require 'amber/tof/writers/latex/test/suite'

require 'amber/cli/environment'
require 'amber/tof/writers/latex/environment'
require 'amber/tof/writers/ascii/environment'

# -------------------------------------------------------------------------- }}}
module Amber
  # {{{ Class WriteFactory documentation.
  #
  # Factory methods used to instantiate Test Plan, Test Suite, Test Cases, Test
  # Steps, and system environment LaTeX or Ascii decorator objects.
  #
  # ------------------------------------------------------------------------ }}}
  class WriterFactory
    # {{{ get_test_plan

    def self.get_test_plan(filename, data, options)
      decoratee = TestPlan.new(filename, data, options)
      if options.writer == 'LaTeX'
        LaTeXTestPlan.new(decoratee)
      else
        AsciiTestPlan.new(decoratee)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_test_suite

    def self.get_test_suite(filename, data, options)
      decoratee = TestSuite.new(filename, data, options)
      if options.writer == 'LaTeX'
        LaTeXTestSuite.new(decoratee)
      else
        AsciiTestSuite.new(decoratee)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_test_case

    def self.get_test_case(filename, data, options)
      decoratee = TestCase.new(filename, data, options)
      if options.writer == 'LaTeX'
        LaTeXTestCase.new(decoratee)
      else
        AsciiTestCase.new(decoratee)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_test_step

    def self.get_test_step(filename, data, options, step, nbr, workingdir)
      decoratee = TestStep.new(filename, data, options, step, nbr, workingdir)
      if options.writer == 'LaTeX'
        LaTeXTestStep.new(decoratee)
      else
        AsciiTestStep.new(decoratee)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_environment

    def self.get_environment(options)
      decoratee = Environment.new
      if options.writer == 'LaTeX'
        LaTeXEnvironment.new(decoratee, options)
      else
        AsciiEnvironment.new(decoratee, options)
      end
    end

    # ---------------------------------------------------------------------- }}}
  end
end
