require 'amber/test'
require 'amber/outputfiles'

module Amber
  class LaTeX_Test < Test
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
      @decoratee.run_command unless @options.dryrun
    end

    def teardown
      Amber::TestEvidence.record_test_name(@macro, @decoratee.options) unless @macro.nil?
      Amber::TestEvidence.close_file(@handle)
      @handle = nil
    end
  end
end
