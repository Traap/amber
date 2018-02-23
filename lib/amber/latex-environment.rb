require 'amber/environment'
require 'amber/outputfiles'

module Amber
  class LaTeX_Environment < Environment

    def initialize(decoratee, language)
      @decoratee = decoratee
      @language = language
      @handle = nil  
    end

    def echo_to_sysout
      @handle = TestEvidence::open_environment_log_file @language
      @handle.write "\\subsection{System Environment}\n"
      @handle.write "\\begin{description}[align=right,leftmargin=*,labelindent=3cm]\n"

      @decoratee.environment.each do |e|
        if e == "PATH" then
          echo_e_to_sysout(e)
        else
          @handle.write "\\item[#{e}:] #{ENV[e]}\n"
        end
      end

      @handle.write "\\end{description}\n"
      TestEvidence::close_file @handle
    end

    def echo_e_to_sysout(e)
      @handle.write "\\item[#{e}:] "
      f = ENV[e].split(':')
      f.each do |g|
        @handle.write "#{g} "
      end
      @handle.write "\n"
    end

  end #LaTeX_Environment
end #Amber
