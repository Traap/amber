require 'amber/test'
require 'amber/factory_method'
require 'amber/test_step'

module Amber
  # Decorate test case output with LaTeX text.
  class TestCase < Test
    def initialize(filename, data, options)
      super('Test Case', filename, data, options)
    end

    def run_command
      workingdir = @data['workingdir']
      nbr = 0
      @data['steps'].each do |s|
        nbr += 1
        step = Amber::TestFactory.get_test_step(@filename,
                                                @data,
                                                @options,
                                                s,
                                                nbr,
                                                workingdir)
        step.process
      end
    end
  end
end
