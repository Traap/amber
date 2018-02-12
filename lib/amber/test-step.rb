require 'amber/test'

module Amber 
  class TestStep < Test
    attr_reader :confirm, :expectation, :command, :evidence

    def initialize(s)
      if s['sudo'] then
        sudo = RbConfig::CONFIG['host_os'] == "cygwin" ? nil : "sudo "
      else
        sudo = nil 
      end
      @confirm = s['confirm']
      @expectation = s['expectation']
      @command = "#{sudo}#{s['command']} #{s['argument']}"
      @evidence = s['evidence']
    end

    def echo_to_sysout(nbr); end

    def run_command
      stdout, stderr, status = Open3.capture3(@command)
    end

  end # TestStep
end # Amber 
