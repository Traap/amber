require 'amber/ascii_test'

module Amber
  # Decorate test suite output with Ascii text.
  class AsciiTestSuite < AsciiTest
    def initialize(decoratee)
      super(decoratee)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call
    end
  end
end
