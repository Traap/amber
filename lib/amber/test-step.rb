require 'amber/test'

module Amber 
  class TestStep < Test
    attr_reader :number, :confirm, :expectation, :command, :evidence

    def initialize(step, number)
      if step['sudo'] then
        sudo = RbConfig::CONFIG['host_os'] == "cygwin" ? nil : "sudo "
      else
        sudo = nil 
      end
      @number = number
      @confirm = step['confirm']
      @expectation = step['expectation']
      @command = "#{sudo}#{step['command']} #{step['argument']}"
      @evidence = step['evidence']
    end

    def echo_to_sysout; end

    def run_command
      stdout, stderr, status = Open3.capture3(@command) if !@options.dryrun
    end

  end # TestStep
end # Amber 
