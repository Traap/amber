# frozen_string_literal: true

require 'amber/cli/environment'
require 'amber/tof/testevidence'

module Amber
  # Decorate environment output with Ascii text.
  class AsciiEnvironment < Amber::Environment
    def initialize(decoratee, options)
      @decoratee = decoratee
      @options = options
      @handle = nil
    end

    def echo_to_sysout
      @handle = Amber::TestEvidence.open_environment_log_file(@options)
      @handle.write "System Environment\n"

      @decoratee.environment.each do |env|
        if env == 'PATH'
          echo_split_path_to_sysout(env)
        else
          @handle.write "  #{env} = #{ENV[env]}\n"
        end
      end

      Amber::TestEvidence.close_file @handle
    end

    def echo_split_path_to_sysout(path)
      @handle.write "  #{path} = "
      ENV[path].split(':').each do |part|
        @handle.write "         #{part}"
      end
      @handle.write "\n"
    end
  end
end
