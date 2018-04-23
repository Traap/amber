require 'amber/ascii_test'

module Amber
  # Decorate test plan output with Ascii text.
  class Ascii_TestPlan < Ascii_Test
    def initialize(decoratee)
      super(decoratee)
    end

    def echo_to_sysout
      method(:echo_to_sysout).super_method.call
    end
  end
end
