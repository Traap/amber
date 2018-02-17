require 'amber/test'
require 'amber/factory-method'
require 'amber/test-step'

module Amber
  class TestCase < Test 

    def initialize(filename, data, options)
      super("Test Case", filename, data, options)
    end

    def run_command
      nbr = 0
      @data['steps'].each do |s|
        nbr = nbr + 1
        step = Amber::TestFactory.get_test_step(@filename, @data, @options, s, nbr)
        step.process
      end
    end

  end # TestCase
end # Amber
