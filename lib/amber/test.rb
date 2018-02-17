module Amber
  class ShellError < StandardError; end

  class Test
    attr_reader :filename, :type, :name, :purpose, :requirement, :options

    def initialize(type, filename, data, options)
      @type = type
      @filename = filename
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
        stdout, stderr, status = Open3.capture3(@command)
      rescue ShellError
        msg = "System command failed: #{status}"
        puts "#{stderr}\n#{msg}\n"
        abort msg
      end
    end

  end # Test
end # Amber orchestrate
