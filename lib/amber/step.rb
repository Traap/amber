module Amber 
  class ShellError < StandardError; end

  class Step  

    def initialize(s)
      if s['sudo'] then
        sudo = "sudo "
      else
        sudo = nil 
      end
      @confirm = s['confirm']
      @expectation = s['expectation']
      @command = "#{sudo}#{s['command']} #{s['argument']}"
      @evidence = s['evidence']
    end

    def echo_to_sysout(nbr)
      puts "         Step: #{nbr}"
      puts "      Confirm: #{@confirm}"
      puts "  Expectation: #{@expectation}"
      puts "      Command: #{@command}"
    end

    def run_command
      begin
        puts "     Evidence: #{@evidence}"
        status = system(@command)
        result = status ? "PASS" : "FAIL"
        puts "  Test Result: #{result}"
        puts ""
      rescue ShellError
        abort "System command failed: #{status}"
      end
    end

  end # Step
end # Amber 
