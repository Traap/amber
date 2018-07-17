require 'amber/test'
require 'amber/outputfiles'

module Amber
  # Base class used to decorate output with LaTeX text.
  class LaTeXTest < Test
    attr_reader :decoratee, :macro

    def initialize(decoratee, macro)
      @macro       = macro
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
