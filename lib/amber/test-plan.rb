require 'amber/test'

module Amber 
  class TestPlan < Test 

    def initialize(filename, data, options)
      super("Test Plan", filename, data, options)
    end

    def run_command; end 

  end # TestPlan
end # Amber 
