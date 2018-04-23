require 'amber/test'

module Amber
  # Decorate test suite output with LaTeX text.
  class TestSuite < Test
    def initialize(filename, data, options)
      super('Test Suite', filename, data, options)
    end

    def run_command; end
  end
end
