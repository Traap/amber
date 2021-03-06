# frozen_string_literal: true
#
# This class orchestrates the Amber workflow.  YAML files are read and parsed
# into Test Plans, Test Suites, Test Cases, and Test Includes.  Each Test object
# is then processed.
# ------------------------------------------------------------------------------

require 'amber/tif/test/case'
require 'amber/tif/test/include'
require 'amber/tif/test/plan'
require 'amber/tif/test/suite'
require 'amber/tof/writers/writer_factory'
require 'amber/workflow'

# ------------------------------------------------------------------------------
module Amber
  # Run the Amber workflow.
  class Workflow

    # --------------------------------------------------------------------------

    def initialize(options)
      @options = options
      @yaml_file = nil
      @test = []
    end

    # --------------------------------------------------------------------------

    def orchestrate
      @options.filename.each do |f|
        parse_yaml_file f
        @test.each(&:process)
      end
    end

    # --------------------------------------------------------------------------

    def parse_yaml_file(filename)
      @test = []

      @yaml_file = YAML.safe_load(File.open(filename))

      @yaml_file.each do |k, v|
        case k
        when 'plan'
          @test << Amber::WriterFactory.get_test_plan(filename, v, @options)
        when 'suite'
          @test << Amber::WriterFactory.get_test_suite(filename, v, @options)
        when 'case'
          @test << Amber::WriterFactory.get_test_case(filename, v, @options)
        when 'includes'
          v.each do |n|
            @test << Include.new(n, @options)
          end
        else
          puts "#{k} is not supported." if @options.verbose
        end
      end
    end

    # --------------------------------------------------------------------------

  end
end
