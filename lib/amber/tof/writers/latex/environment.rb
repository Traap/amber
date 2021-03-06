# frozen_string_literal: true

require 'amber/cli/environment'
require 'amber/tof/testevidence'

module Amber
  # Decorate environment output with LaTeX text.
  class LaTeXEnvironment < Amber::Environment
    def initialize(decoratee, options)
      @decoratee = decoratee
      @options = options
      @handle = nil
    end

    def echo_to_sysout
      @handle = TestEvidence.open_environment_log_file(@options)
      write_subsection
      @handle.write "\\begin{description}[align=right,leftmargin=*,labelindent=5cm]\n"

      @decoratee.environment.each do |e|
        if e.downcase.include?('path')
          echo_e_to_sysout(e)
        else
          i = replace_characters(e)
          v = replace_characters((ENV[e]).to_s)
          @handle.write "\\item[#{i}:] #{v}\n"
        end
      end

      @handle.write "\\end{description}\n"
      TestEvidence.close_file @handle
    end

    def echo_e_to_sysout(e)
      i = replace_characters(e)
      @handle.write "\\item[#{i}:] See below.\n"
      @handle.write "\\begin{enumerate}\n"
      if ENV[e].nil?
        @handle.write "\\item nil\n"
      else
        f = ENV[e].split(':')
        f.each do |g|
          v = replace_characters(g)
          @handle.write "\\item #{v}\n"
        end
      end
      @handle.write "\\end{enumerate}\n"
    end

    private

    def write_subsection
      @handle.write "\\subsection{System Environment}\n"
      @handle.write "The hyphen character replaced the underscore character,\n"
      @handle.write "and the forward slash character replaced the backslash\n"
      @handle.write "character throughout this section.\n"
    end

    def replace_characters(text)
      escape_characters(
        replace_backslash_with_forwardslash(
          replace_underscore_with_hyphen(text)
        )
      )
    end

    def escape_characters(text)
      text.gsub(/#/, '\#').gsub(/\$/, '\$')
    end

    def replace_underscore_with_hyphen(text)
      text.tr('_', '-')
    end

    def replace_backslash_with_forwardslash(text)
      text.tr('\\', '/')
    end
  end
end
