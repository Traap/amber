module Amber
  class ShellError < StandardError; end

  class Test
    attr_reader :type, :name, :purpose, :requirement

    def initialize(type, data, options)
      @type = type
      @name = data['name']
      @purpose = data['purpose']
      @requirement = data['requirement']
      @options = options
      @command = nil
    end

    def process
      echo_to_sysout
      run_command
    end

    protected
    def echo_to_sysout 
      name = "#{@type}: ".rjust(15) << "#{@name}"
      puts "#{name}"
      puts "      Purpose: #{@purpose}"
      puts "  Requirement: #{@requirement}"
    end

    def run_command
      begin
        puts "      Command: #{@command}"
        status = system(@command)
      rescue ShellError
        abort "System command failed: #{status}"
      end
    end

  end # Test
end # Amber 
