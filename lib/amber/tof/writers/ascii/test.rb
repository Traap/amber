# frozen_string_literal: true

require 'amber/tif/test'
require 'amber/tof/testevidence'

module Amber
  # Base class used to decorate output with Ascii text.
  class AsciiTest < Amber::Test
    attr_reader :decoratee, :macro

    def initialize(decoratee)
      @macro = if decoratee.type == 'Test Step'
                 nil
               else
                 "#{decoratee.type}: #{File.basename(decoratee.filename, '.*')}\n"
               end
      @decoratee   = decoratee
      @handle      = nil

      @command     = decoratee.command
      @data        = decoratee.data
      @filename    = decoratee.filename
      @name        = decoratee.name
      @options     = decoratee.options
      @purpose     = decoratee.purpose
      @requirement = decoratee.requirement
      @type        = decoratee.type
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
      @decoratee.run_command if @options.run?
    end

    def teardown
      Amber::TestEvidence.close_file(@handle)
      @handle = nil
    end
  end
end
