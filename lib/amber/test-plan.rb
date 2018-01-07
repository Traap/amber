module Amber 
  class TestPlan < Node 

    def initialize(data, options)
      super("Test Plan", data, options)
    end

    def run_command; end 

  end # TestPlan
end # Amber 
