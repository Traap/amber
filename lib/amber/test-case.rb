module Amber
  class TestCase < Node

    def initialize(data, options)
      super("Test Case", data, options)
    end

    def process
      method(:process).super_method.call
      @data['steps'].each do |s|
        step = Step.new(s)
        step.echo_to_sysout
        step.run_command  if !@options.dryrun
      end
    end

  end # TestCase
end # Amber 
