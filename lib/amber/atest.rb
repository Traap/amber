module Amber
  class ShellError < StandardError; end

  class Test

    def initialize(type, data, options)
      @data = data
      @options = options
      @testtype = TestType.new(type, data)
      @command = nil
    end

    def process
      echo_to_sysout
      run_command
    end

    protected
    def echo_to_sysout 
      @testtype.echo_to_sysout
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
