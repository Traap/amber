# frozen_string_literal: true

# {{{ Required files.

require 'amber/cli/options'
require 'amber/utility/substitute'

# -------------------------------------------------------------------------- }}}
module Amber
  # {{{ Class ShellError

  class ShellError < StandardError; end

  # ------------------------------------------------------------------------ }}}
  # {{{ Class Test documentation.
  #
  # The base class for all Test.  Amber process YAML files such as:
  #   Test Plan
  #     A plan typically references one or more Test Suites.
  #
  #   Test Suite
  #     A suite will reference one or more Test Cases.
  #
  #   Test Case
  #     A test case has one or more test step.  A test step may invoke an
  #     external command.
  #
  #   Include
  #     A reference to a YAML file somewhere in your path.
  #
  #------------------------------------------------------------------------- }}}
  class Test
    # {{{ Attributes

    attr_reader :command, :data, :filename, :name, :options, :purpose,
                :requirement, :type

    # ---------------------------------------------------------------------- }}}
    # {{{ initialize

    def initialize(type, filename, data, options)
      @command = nil
      @data = data
      @filename = filename
      @name = Amber::Substitute.strings(@filename, options, data['name'])
      @options = options
      @purpose = Amber::Substitute.strings(@filename, options, data['purpose'])
      @requirement = Amber::Substitute.strings(@filename, options, data['requirement'])
      @type = type
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ process

    def process
      setup
      echo_to_sysout
      run_command         if @options.run?
      record_requirements if @options.log_requirement?
      teardown
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ protected section

    protected

    # ---------------------------------------------------------------------- }}}
    # {{{ setup

    def setup; end

    # ---------------------------------------------------------------------- }}}
    # {{{ teardown

    def teardown; end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_to_sysout

    def echo_to_sysout
      name = "#{@type}: ".rjust(15) << @name.to_s
      puts name.to_s if @options.verbose?
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ run_command

    def run_command
      puts @command.to_s if @options.verbose?
      status = system(@command)
    rescue ShellError
      msg = "System command failed: #{status}"
      puts "#{msg}\n" if @options.verbose
      abort msg
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ record_requirements

    def record_requirements
      Amber::TestEvidence.record_requirement_tested(filename, requirement) unless requirement.nil?
    end

    # ---------------------------------------------------------------------- }}}
  end
end
