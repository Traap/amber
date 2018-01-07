module Amber
  class TestSuite < Node

    def initialize(data, options)
      super("Test Suite", data, options)
    end

    def run_command; end 

  end # TestSuite
end # Amber 
