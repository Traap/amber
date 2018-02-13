require 'amber/test'
require 'amber/factory-method'
require 'amber/test-step'

module Amber
  class TestCase < Test 

    def initialize(data, options)
      super("Test Case", data, options)
    end

    def run_command
      nbr = 0
      tf = TestFactory.new()
      @data['steps'].each do |s|
        nbr = nbr + 1
        step = tf.get_test_step(@options.writer, TestStep.new(s, nbr))
        step.echo_to_sysout
        step.run_command  if !@options.dryrun
      end
    end

  end # TestCase
end # Amber
