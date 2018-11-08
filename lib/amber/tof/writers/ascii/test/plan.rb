# frozen_string_literal: true

require 'amber/tof/writers/ascii/test'

module Amber
  # Decorate test plan output with Ascii text.
  class AsciiTestPlan < Amber::AsciiTest
    def initialize(decoratee)
      super(decoratee)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call
    end
  end
end
