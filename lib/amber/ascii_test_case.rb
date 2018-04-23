require 'amber/ascii_test'

module Amber
  # Decorate test case output with Ascii text.
  class AsciiTestCase < AsciiTest
    def initialize(decoratee)
      super(decoratee)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call
    end
  end
end
