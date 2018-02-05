module Amber 
  class NodeType  

    def initialize(type, data)
      @type = type
      @name = data['name']
      @purpose = data['purpose']
      @requirement = data['requirement']
    end

    def echo_to_sysout
      name = "#{@type}: ".rjust(15) << "#{@name}"
      puts "#{name}"
      puts "      Purpose: #{@purpose}"
      puts "  Requirement: #{@requirement}"
    end

  end # NodeType
end # Amber 
