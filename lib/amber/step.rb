module Amber 
  class ShellError < StandardError; end

  class Step  

    def initialize(s)
      if s['sudo'] then
        sudo = "sudo "
      else
        sudo = nil 
      end
      @command = "#{sudo}#{s['command']} #{s['argument']}"
    end

    def echo_to_sysout
      puts "      Command: #{@command}"
    end

    def run_command
      begin
        status = system(@command)
      rescue ShellError
        abort "System command failed: #{status}"
      end
    end

  end # Step
end # Amber 
