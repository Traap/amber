require 'amber/Environment'
require 'amber/factory-method'
require 'amber/include'
require 'amber/test-case'
require 'amber/test-plan'
require 'amber/test-suite'
require 'amber/workflow'

module Amber 
  class Workflow

    def initialize options
      @options = options
      @yaml_file = nil
      @test = []
    end

    def orchestrate
      @options.filename.each do |f|
        parse_yaml_file f
        @test.each do |n|
          n.process
        end
      end
      Environment.new().echo_to_sysout if !@options.equipment
    end

    def parse_yaml_file(yaml_file)
      @test = []

      @yaml_file = YAML.load(File.open(yaml_file))

      @yaml_file.each do |k,v|
        case k
        when "plan"
          @test << Amber::TestFactory.get_test_plan(v, @options)
        when "suite"
          @test << Amber::TestFactory.get_test_suite(v, @options)
        when "case"
          @test << Amber::TestFactory.get_test_case(v, @options)
        when "includes"
          v.each do |n|
            @test << Include.new(n, @options)
          end
        else
          puts "#{k} is not supported."
        end
      end
    end

  end # Workflow
end # Amber 
