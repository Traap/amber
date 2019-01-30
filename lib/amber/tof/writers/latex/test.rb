# frozen_string_literal: true

require 'amber/tif/test'
require 'amber/tof/testevidence'

module Amber
  # Base class used to decorate output with LaTeX text.
  class LaTeXTest < Amber::Test
    attr_reader :decoratee, :macro

    def initialize(decoratee, macro)
      @macro       = macro
      @decoratee   = decoratee
      @handle      = nil

      # @command     = decoratee.command
      # @data        = decoratee.data
      # @filename    = decoratee.filename
      # @name        = decoratee.name
      # @options     = decoratee.options
      # @purpose     = decoratee.purpose
      # @requirement = decoratee.requirement
      # @type        = decoratee.type
    end
    
    def command
      @decoratee.command
    end

    def data
      @decoratee.data
    end

    def filename
      @decoratee.filename
    end

    def name
      @decoratee.name
    end

    def options
      @decoratee.options
    end

    def purpose
      @decoratee.purpose
    end

    def requirement
      @decoratee.requirement
    end

    def type
      @decoratee.type
    end

    def setup
      Amber::TestEvidence.record_test_name(@macro, @decoratee.options) unless @macro.nil?
      @handle = Amber::TestEvidence.open_log_file(@decoratee.filename,
                                                  @decoratee.options)
    end

    def echo_to_sysout(outline_level)
      @decoratee.echo_to_sysout

      name = "#{@decoratee.type}: #{@decoratee.name}"
      @handle.write "\\#{outline_level}{#{name}}\n"
      @handle.write "\\begin{description}[align=right,leftmargin=*,labelindent=3cm]\n"
      @handle.write "\\item[Purpose:] #{@decoratee.purpose}\n"
      @handle.write "\\item[Requirement:] #{@decoratee.requirement}\n" unless @decoratee.requirement.nil?
      @handle.write "\\end{description}\n"
      @handle.flush
    end

    def run_command
      @decoratee.run_command if @options.okay_to_run?
    end

    def teardown
      Amber::TestEvidence.close_file(@handle)
      @handle = nil
    end
  end
end
