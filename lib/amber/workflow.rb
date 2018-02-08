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
          @test << TestPlan.new(v, @options)
        when "suite"
          @test << TestSuite.new(v, @options)
        when "case"
          @test << TestCase.new(v, @options)
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
