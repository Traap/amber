# frozen_string_literal: true

require 'amber/tif/test'

module Amber
  # A test plan can contain one or more test suites.
  class TestPlan < Test
    def initialize(filename, data, options)
      super('Test Plan', filename, data, options)
    end

    def run_command; end
  end
end
