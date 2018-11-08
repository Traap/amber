# frozen_string_literal: true

require 'amber/cli/options'
require 'amber/utility/substitute'

module Amber
  # Shell Error
  class ShellError < StandardError; end

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
  class Test
    attr_reader :command, :data, :filename, :name, :options, :purpose,
                :requirement, :type

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

    def process
      setup
      echo_to_sysout
      run_command if @options.okay_to_run?
      teardown
    end

    protected

    def setup; end

    def teardown; end

    def echo_to_sysout
      name = "#{@type}: ".rjust(15) << @name.to_s
      puts name.to_s if @options.verbose
    end

    def run_command
      puts @command.to_s if @options.verbose
      status = system(@command)
    rescue ShellError
      msg = "System command failed: #{status}"
      puts "#{msg}\n" if @options.verbose
      abort msg
    end
  end
end
