require 'amber/test'
require 'amber/outputfiles'

module Amber
  # Base class used to decorate output with Ascii text.
  class AsciiTest < Test
    attr_reader :decoratee, :macro

    def initialize(decoratee)
      @macro = if decoratee.type == 'Test Step'
                 nil
               else
                 decoratee.type + ': ' +
                   File.basename(decoratee.filename, '.*') + "\n"
               end
      @decoratee   = decoratee
      @type        = decoratee.type
      @filename    = decoratee.filename
      @data        = decoratee.data
      @name        = decoratee.name
      @purpose     = decoratee.purpose
      @requirement = decoratee.requirement
      @options     = decoratee.options
      @command     = decoratee.command
      @handle      = nil
    end

    def setup
      Amber::TestEvidence.record_test_name(@macro, @decoratee.options) unless @macro.nil?
      @handle = Amber::TestEvidence.open_log_file(@decoratee.filename,
                                                  @decoratee.options)
    end

    def echo_to_sysout
      @decoratee.echo_to_sysout

      name = "#{@decoratee.type}: ".rjust(15) << @decoratee.name.to_s
      @handle.write "#{name}\n"
      @handle.write "      Purpose: #{@decoratee.purpose}\n"
      @handle.write "  Requirement: #{@decoratee.requirement}\n\n" unless @decoratee.requirement.nil?
      @handle.flush
    end

    def run_command
      @decoratee.run_command if @options.okay_to_run?
    end

    def teardown
      Amber::TestEvidence.record_test_name(@macro, @decoratee.options) unless @macro.nil?
      Amber::TestEvidence.close_file(@handle)
      @handle = nil
    end
  end
end
