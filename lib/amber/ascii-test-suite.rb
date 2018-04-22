require 'amber/ascii-test'

module Amber
  class Ascii_TestSuite < Ascii_Test
    def initialize(decoratee)
      super(decoratee)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call
    end
  end # Ascii_TestSuite
end # Amber
