module Amber 
# ------------------------------------------------------------------------------
class Workflow
  def initialize options
    @options = options
    @yaml_file = nil
    @commands = []
  end

# ------------------------------------------------------------------------------
  def orchestrate
    @options.filename.each do |f|
      parse_yaml_file f
      @commands.each do |c|
        c.remove_artifact
        c.install_artifact
      end
    end
  end

# ------------------------------------------------------------------------------
  def parse_yaml_file(yaml_file)
    puts "Parsing #{yaml_file}"
    @commands = []
    @yaml_file = YAML.load(File.open(yaml_file))

    @yaml_file.each do |k,v|
      case k
      when "plan"
        @commands << TestPlan.new(v, @options)
      when "suite"
        @commands << TestSuite.new(v, @options)
      when "case"
        @commands << TestCase.new(v, @options)
      when "includes"
        v.each do |n|
          @commands << Include.new(n, @options)
        end
      else
        puts "#{k} is not supported."
      end
    end
  end

# ------------------------------------------------------------------------------
end # End Workflow
# ------------------------------------------------------------------------------
end # module
# ------------------------------------------------------------------------------
