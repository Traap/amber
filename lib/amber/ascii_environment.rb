require 'amber/environment'
require 'amber/outputfiles'

module Amber
  # Decorate environment output with Ascii text.
  class AsciiEnvironment < Environment
    def initialize(decoratee, options)
      @decoratee = decoratee
      @options = options
      @handle = nil
    end

    def echo_to_sysout
      @handle = TestEvidence.open_environment_log_file(@options)
      @handle.write "System Environment\n"

      @decoratee.environment.each do |env|
        if env == 'PATH'
          echo_split_path_to_sysout(env)
        else
          @handle.write "  #{env} = #{ENV[env]}\n"
        end
      end

      TestEvidence.close_file @handle
    end

    def echo_split_path_to_sysout(path)
      @handle.write "  #{path} = "
      ENV[path].split(':').each do |ggg|
        @handle.write "         #{ggg}"
      end
      @handle.write "\n"
    end
  end
end
