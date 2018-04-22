require 'amber/test'

module Amber
  class TestSuite < Test
    def initialize(filename, data, options)
      super('Test Suite', filename, data, options)
    end

    def run_command; end
  end # TestSuite
end # Amber
