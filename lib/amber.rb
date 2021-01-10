# frozen_string_literal: true

# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause
#
# This program is used to automate testing of command line programs.  Amber uses
# a JUnit like framework in that it understands test suites and test plans.
# Amber provides a robust reporting mechanism that is used to product objective
# evidence a component has met its intended use.
#
# Amber processes YAML files to run test plans, test suites, and test cases in
# order to assemble records that meet Quality Systems Regulations requirements
# for documented evidence.
# ------------------------------------------------------------------------------

require 'amber/initialize'

# ------------------------------------------------------------------------------
module Amber
  # Command Line Interface is used to parse command line options, run the Amber
  # workflow, and record the environment Amber was run with.
  class CLI
    # {{{ Execute the command ling with ARGV and options.

    def execute(argv)
      opts = CommandLineOptions.parse argv
      dump_parsed_data_and_exit(argv, opts) if opts.data[:dump]
      Amber::TestEvidence.obliterate_test_output() if opts.okay_to_obliterate?
      Workflow.new(opts).orchestrate
      Amber::WriterFactory.get_environment(opts).echo_to_sysout if opts.okay_to_echo_env?
    end

   # ----------------------------------------------------------------------- }}}
    # {{{ Dump ARGV and options exit.

    def dump_parsed_data_and_exit(argv, opts)
      puts "Amber is exiting!"
      puts ""
      puts "ARGV:"
      puts ""
      pp argv
      puts "Options:"
      puts ""
      pp opts
      exit
    end

    # ---------------------------------------------------------------------- }}}
  end
end
# ------------------------------------------------------------------------------
