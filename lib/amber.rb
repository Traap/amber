# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause
#
# This program is used to automate testing of command line programs.  Amber uses
# a JUnit like framework in that it understands test suites and test plans.
# Amber provides a robust reporting mechanism that is used to product objective
# evidence a component has met its intended use.
#
# Amber process YAML files to run test plans, test suites, and test cases in
# order to assemble records that meet Quality Systems Regulations requirements
# for documented evidence.
# ------------------------------------------------------------------------------

require 'amber/environment'
require 'amber/factory-method'
require 'amber/initialize'
require 'amber/options'
require 'amber/workflow'

# ------------------------------------------------------------------------------
module Amber
  # Command Line Interface is used to parse command line options, run the Amber
  # workflow, and record the environment Amber was run with.
  class CLI
    def execute(args)
      options = CommandLineOptions.parse args

      if options

        Workflow.new(options).orchestrate

        if options.environment

          Amber::TestFactory
            .get_environment(options)
            .echo_to_sysout

        end

      end
    end
  end
end
# ------------------------------------------------------------------------------
