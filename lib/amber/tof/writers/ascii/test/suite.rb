# frozen_string_literal: true

require 'amber/tof/writers/ascii/test'

module Amber
  # Decorate test suite output with Ascii text.
  class AsciiTestSuite < Amber::AsciiTest
    def echo_to_sysout
      method(:echo_to_sysout).super_method.call
    end
  end
end
