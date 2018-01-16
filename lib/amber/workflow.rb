module Amber 
  class Workflow

    def initialize options
      @options = options
      @yaml_file = nil
      @node = []
    end

    def orchestrate
      @options.filename.each do |f|
        parse_yaml_file f
        @node.each do |n|
          n.process
        end
      end
      Environment.new().echo_to_sysout
    end

    def parse_yaml_file(yaml_file)
      @node = []
      @yaml_file = YAML.load(File.open(yaml_file))

      @yaml_file.each do |k,v|
        case k
        when "plan"
          @node << TestPlan.new(v, @options)
        when "suite"
          @node << TestSuite.new(v, @options)
        when "case"
          @node << TestCase.new(v, @options)
        when "includes"
          v.each do |n|
            @node << Include.new(n, @options)
          end
        else
          puts "#{k} is not supported."
        end
      end
    end

  end # Workflow
end # Amber 
