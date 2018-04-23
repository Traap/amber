require 'amber/environment'
require 'amber/outputfiles'

module Amber
  class Ascii_Environment < Environment
    def initialize(decoratee, options)
      @decoratee = decoratee
      @options = options
      @handle = nil
    end

    def echo_to_sysout
      @handle = TestEvidence.open_environment_log_file(@options)
      @handle.write "System Environment\n"
      @handle.write "\\begin{description}[align=right,leftmargin=*,labelindent=3cm]\n"

      @decoratee.environment.each do |e|
        if e == 'PATH'
          echo_e_to_sysout(e)
        else
          @handle.write "  #{e} = #{ENV[e]}\n"
        end
      end

      TestEvidence.close_file @handle
    end

    def echo_e_to_sysout(e)
      @handle.write "  #{e} = "
      f = ENV[e].split(':')
      f.each do |g|
        @handle.write "         #{g}"
      end
      @handle.write "\n"
    end
  end
end
