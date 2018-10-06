# frozen_string_literal: true

require 'amber/tif/test'

module Amber
  # Decorate test plan output with LaTeX text.
  class TestPlan < Test
    def initialize(filename, data, options)
      super('Test Plan', filename, data, options)
    end

    def run_command; end
  end
end
