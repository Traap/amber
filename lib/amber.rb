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
      opt = CommandLineOptions.parse argv
      dump_argv_and_options_and_exit argv if opt.data[:dump]
      Amber::TestEvidence.obliterate_test_output() if opt.okay_to_obliterate?
      Workflow.new(opt).orchestrate
      Amber::WriterFactory.get_environment(opt).echo_to_sysout if opt.okay_to_echo_env?
    end

   # ----------------------------------------------------------------------- }}}
    # {{{ Dump ARGV and options exit.

    def dump_argv_and_options_and_exit(argv)
      puts "Amber is exiting!"
      puts ""
      puts "Command Line Option values:"
      puts "    browser: #{@clo.options.data[:browser]}"
      puts "     dryrun: #{@clo.options.data[:dryrun]}"
      puts "environment: #{@clo.options.data[:environment]}"
      puts "   filename: #{@clo.options.data[:filename]}"
      puts "   language: #{@clo.options.data[:language]}"
      puts "log-command: #{@clo.options.data[:log_command]}"
      puts " obliterate: #{@clo.options.data[:obliterate]}"
      puts "requirement: #{@clo.options.data[:requirement]}"
      puts "   simulate: #{@clo.options.data[:simulate]}"
      puts "    verbose: #{@clo.options.data[:verbose]}"
      puts "     writer: #{@clo.options.data[:writer]}"
      puts ""
      puts "Command Line Option function values:"
      puts "             okay_to_run?: #{@clo.options.okay_to_run?}"
      puts "      okay_to_obliterate?: #{@clo.options.okay_to_obliterate?}"
      puts "okay_to_list_requirement?: #{@clo.options.okay_to_list_requirement?}"
      puts "     okay_to_log_command?: #{@clo.options.okay_to_log_command?}"
      puts "            has_language?: #{@clo.options.has_language?}"
      puts "             has_browser?: #{@clo.options.has_browser?}"
      puts ""
      puts "ARGV:"
      pp argv
      puts "Options:"
      puts ""
      pp opt
      exit
    end

    # ---------------------------------------------------------------------- }}}
  end
end
# ------------------------------------------------------------------------------
