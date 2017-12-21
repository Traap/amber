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

require 'amber/initialize'

# ------------------------------------------------------------------------------
module Amber 
  class CLI
    def execute(args)
      puts "Parsing command line"
      options = CommandLineOptions.parse args
      if options
        puts "Workflow initialized"
        workflow = Workflow.new(options)

        puts "Workflow orchestration."
        workflow.orchestrate
      end
    end
  end
end
# ------------------------------------------------------------------------------
