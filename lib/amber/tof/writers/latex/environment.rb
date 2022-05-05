# frozen_string_literal: true

require 'amber/cli/environment'
require 'amber/tof/testevidence'

module Amber
  # Decorate environment output with LaTeX text.
  class LaTeXEnvironment < Amber::Environment
    # {{{ initialize

    def initialize(decoratee, options)
      super()
      @decoratee = decoratee
      @options = options
      @handle = nil
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_to_sysout

    def echo_to_sysout
      @handle = TestEvidence.open_environment_log_file(@options)
      write_subsection
      @handle.write "\\begin{description}[align=right,leftmargin=*,labelindent=5cm]\n"
      echo_to_sysout_items
      @handle.write "\\end{description}\n"
      TestEvidence.close_file @handle
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_to_sysout_items

    def echo_to_sysout_items
      @decoratee.environment.each do |e|
        if e.downcase.include?('path')
          echo_e_to_sysout(e)
        else
          i = replace_characters(e)
          v = replace_characters((ENV[e]).to_s)
          @handle.write "\\item[#{i}:] #{v}\n"
        end
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_e_to_sysout

    def echo_e_to_sysout(env)
      i = replace_characters(env)
      @handle.write "\\item[#{i}:] See below.\n"
      @handle.write "\\begin{enumerate}\n"
      echo_e_to_sysout_items(env)
      @handle.write "\\end{enumerate}\n"
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ echo_e_to_sysout_items

    def echo_e_to_sysout_items(env)
      if ENV[env].nil?
        @handle.write "\\item nil\n"
      else
        f = ENV[env].split(':')
        f.each do |g|
          v = replace_characters(g)
          @handle.write "\\item #{v}\n"
        end
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ private section

    private

    # ---------------------------------------------------------------------- }}}
    # {{{ write_subsection

    def write_subsection
      @handle.write "\\subsection{System Environment}\n"
      @handle.write "The hyphen character replaced the underscore character,\n"
      @handle.write "and the forward slash character replaced the backslash\n"
      @handle.write "character throughout this section.\n"
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ replace_characters

    def replace_characters(text)
      escape_characters(
        replace_backslash_with_forwardslash(
          replace_underscore_with_hyphen(text)
        )
      )
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ replace_characters

    def escape_characters(text)
      text.gsub(/#/, '\#').gsub(/\$/, '\$')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ replace_underscore_with_hyphen

    def replace_underscore_with_hyphen(text)
      text.tr('_', '-')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ replace_backslash_with_forwardslash

    def replace_backslash_with_forwardslash(text)
      text.tr('\\', '/')
    end

    # ---------------------------------------------------------------------- }}}
  end
end
