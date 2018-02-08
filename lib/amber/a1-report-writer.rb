module Amber 
  class ReportWriter < Test 
    attr_reader :adaptee

    def initialize(adaptee)
      @adaptee = adaptee
    end

    def run_command; end 

  end # ReportWriter 
end # Amber 
