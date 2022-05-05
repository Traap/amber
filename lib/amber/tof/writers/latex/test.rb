# frozen_string_literal: true

# {{{ Required files.

require 'amber/tif/test'
require 'amber/tof/testevidence'
require 'amber/tof/writers/latex/string_to_latex'

# -------------------------------------------------------------------------- }}}
module Amber
  # Base class used to decorate output with LaTeX text.
  class LaTeXTest < Amber::Test
    # {{{ Attributes

    attr_reader :decoratee, :macro

    # ---------------------------------------------------------------------- }}}
    # {{{ initialize

    def initialize(decoratee, macro)
      super(decoratee.type, decoratee.filename, decoratee.data, decoratee.options)

      @macro       = macro
      @decoratee   = decoratee
      @handle      = nil

      @command     = decoratee.command
      @name        = decoratee.name
      @purpose     = decoratee.purpose
      @requirement = decoratee.requirement
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ setup

    def setup
      Amber::TestEvidence.record_test_name(@macro, @decoratee.options) unless @macro.nil?
      @handle = Amber::TestEvidence.open_log_file(@decoratee.filename,
                                                  @decoratee.options)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_to_sysout

    def echo_to_sysout(outline_level)
      @decoratee.echo_to_sysout

      name = "#{@decoratee.type}: #{@decoratee.name}"
      @handle.write "\\#{outline_level}{#{name}}\n"
      @handle.write "\\begin{description}[align=right,leftmargin=3.2cm,labelindent=3.0cm]\n"
      @handle.write "\\item[Purpose:] #{toLaTeX(@decoratee.purpose)}\n"
      @handle.write "\\item[Requirement:] #{toLaTeX(@decoratee.requirement)}\n" unless @decoratee.requirement.nil?
      @handle.write "\\end{description}\n"
      @handle.flush
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ run_command

    def run_command
      @decoratee.run_command if @options.run?
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ teardown

    def teardown
      Amber::TestEvidence.close_file(@handle)
      @handle = nil
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ toLaTeX

    # rubocop:disable Naming.MethodName

    def toLaTeX(string)
      Amber::StringToLaTeX.convert(string)
    end
    # rubocop:enable Naming.MethodName

    # ---------------------------------------------------------------------- }}}
  end
end
