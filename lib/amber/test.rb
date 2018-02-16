module Amber
  class ShellError < StandardError; end

  class Test
    attr_reader :type, :name, :purpose, :requirement, :options

    def initialize(type, data, options)
      @type = type
      @data = data
      @name = data['name']
      @purpose = data['purpose']
      @requirement = data['requirement']
      @options = options
      @command = nil
    end

    def process
      setup
      echo_to_sysout
      run_command
      teardown
    end

    protected
    def setup; end

    def teardown; end

    def echo_to_sysout 
      name = "#{@type}: ".rjust(15) << "#{@name}"
      puts "#{name}"
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
