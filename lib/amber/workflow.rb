# frozen_string_literal: true

# {{{ Workflow orchestration.

# This class orchestrates the Amber workflow.  YAML files are read and parsed
# into Test Plans, Test Suites, Test Cases, and Test Includes.  Each Test object
# is then processed.

# -------------------------------------------------------------------------- }}}
# {{{ Required files.

require 'amber/tif/test/case'
require 'amber/tif/test/include'
require 'amber/tif/test/plan'
require 'amber/tif/test/suite'
require 'amber/tof/writers/writer_factory'
require 'amber/workflow'

# -------------------------------------------------------------------------- }}}
module Amber
  # {{{ Run the Amber workflow. }}}
  class Workflow
    # {{{ initializ

    def initialize(options)
      @options = options
      @yaml_file = nil
      @filename = nil
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ orchestrate

    def orchestrate
      @options.data[:files].each do |f|
        parse_yaml_file f
        @test.each(&:process)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ parse_yaml_fil

    def parse_yaml_file(filename)
      @test = []
      @filename = filename
      @yaml_file = YAML.safe_load(File.open(@filename))
      process_yaml_file
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ process_yaml_file

    def process_yaml_file
      @yaml_file.each do |key, value|
        validate_key(key, value)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ validate_key

    def validate_key(key, value)
      if %w[plan suite case includes].include? key
        process_command(key, value)
      else
        puts "#{key} is not supported."
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ process_command

    def process_command(key, value)
      case key
      when 'plan'
        @test << get_test_plan(value)
      when 'suite'
        @test << get_test_suite(value)
      when 'case'
        @test << get_test_case(value)
      else
        get_include(value)
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_test_plan

    def get_test_plan(value)
      Amber::WriterFactory.get_test_plan(@filename, value, @options)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_test_suite

    def get_test_suite(value)
      Amber::WriterFactory.get_test_suite(@filename, value, @options)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_test_case

    def get_test_case(value)
      Amber::WriterFactory.get_test_case(@filename, value, @options)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_include

    def get_include(value)
      value.each do |node|
        @test << Include.new(node, @options)
      end
    end

    # ---------------------------------------------------------------------- }}}
  end
end
