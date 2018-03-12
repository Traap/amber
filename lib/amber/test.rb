module Amber
  class ShellError < StandardError; end

  class Test
    attr_reader :type, :filename, :data, :name, :purpose,
                :requirement, :options, :command

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
