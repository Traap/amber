# frozen_string_literal: true

require 'amber/tif/test'

module Amber
  # Decorate test suite output with LaTeX text.
  class TestSuite < Amber::Test
    def initialize(filename, data, options)
      super('Test Suite', filename, data, options)
    end

    def run_command; end
  end
end
