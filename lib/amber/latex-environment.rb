require 'amber/environment'
require 'amber/outputfiles'

module Amber
  class LaTeX_Environment < Environment

    def initialize(decoratee, options)
      @decoratee = decoratee
      @options = options 
      @handle = nil  
    end

    def echo_to_sysout
      @handle = TestEvidence::open_environment_log_file(@options)
      write_subsection
      @handle.write "\\begin{description}[align=right,leftmargin=*,labelindent=3cm]\n"

      @decoratee.environment.each do |e|
        if e.downcase.include?("path") then
          echo_e_to_sysout(e)
        else
          i = replace_underscore_with_hyphen(e)
          v = replace_backslash_with_forwardslash("#{ENV[e]}")
          @handle.write "\\item[#{i}:] #{v}\n"
        end
      end

      @handle.write "\\end{description}\n"
      TestEvidence::close_file @handle
    end

    def echo_e_to_sysout(e)
      i = replace_underscore_with_hyphen(e) 
      @handle.write "\\item[#{i}:] See below.\n"
      @handle.write "\\begin{enumerate}\n"
      f = ENV[e].split(':')
      f.each do |g|
        v = replace_backslash_with_forwardslash(g)
        @handle.write "\\item #{v}\n"
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

    def replace_underscore_with_hyphen(text)
      text.tr("_", "-")
    end

    def replace_backslash_with_forwardslash(text)
      text.tr("\\", "/")
    end

  end #LaTeX_Environment
end #Amber
