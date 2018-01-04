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
      @nodetype.echo_to_sysout
    end

    protected
    def run_command
      begin
        status = system(@command)
      rescue ShellError
        abort "System command failed: #{status}"
      end
    end

  end # Node
end # Amber 
