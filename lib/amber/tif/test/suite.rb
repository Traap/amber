# frozen_string_literal: true

require 'amber/tif/test'

module Amber
  # A test suite is a collection of one or more test cases. 
  class TestSuite < Amber::Test
    def initialize(filename, data, options)
      super('Test Suite', filename, data, options)
    end

    def run_command; end
  end
end
