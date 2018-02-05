module Amber
  class ShellError < StandardError; end

  class Node

    def initialize(type, data, options)
      @data = data
      @options = options
      @nodetype = NodeType.new(type, data)
      @command = nil
    end

    def process
      echo_to_sysout
      run_command
    end

    protected
    def echo_to_sysout 
      @nodetype.echo_to_sysout
    end

    def run_command
      begin
        puts "      Command: #{@command}"
        status = system(@command)
      rescue ShellError
        abort "System command failed: #{status}"
      end
    end

  end # Node
end # Amber 
