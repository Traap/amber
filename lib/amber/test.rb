require 'amber/substitute'

module Amber
  class ShellError < StandardError; end

  class Test
    attr_reader :command, :data, :filename, :name, :options, :purpose,
                :requirement, :type

    def initialize(type, filename, data, options)
      @command = nil
      @data = data
      @filename = filename
      @name = Amber::Substitute.strings(options, data['name'])
      @options = options
      @purpose = Amber::Substitute.strings(options, data['purpose'])
      @requirement = Amber::Substitute.strings(options, data['requirement'])
      @type = type
    end

    def process
      setup
      echo_to_sysout
      run_command if !@options.dryrun
      teardown
    end

    protected
    def setup; end

    def teardown; end

    def echo_to_sysout
      name = "#{@type}: ".rjust(15) << "#{@name}"
      puts "#{name}" if @options.verbose
    end

    def run_command
      begin
        puts "#{@command}" if @options.verbose
        status = system(@command)
      rescue ShellError
        msg = "System command failed: #{status}"
        puts "#{msg}\n" if @options.verbose
        abort msg
      end
    end

  end # Test
end # Amber orchestrate
