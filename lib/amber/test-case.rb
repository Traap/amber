module Amber
  class TestCase < Node

    def initialize(data, options)
      super("Test Case", data, options)
    end

    def run_command
      nbr = 0
      @data['steps'].each do |s|
        nbr = nbr + 1
        step = Step.new(s)
        step.echo_to_sysout nbr
        step.run_command  if !@options.dryrun
      end
    end

  end # TestCase
end # Amber
