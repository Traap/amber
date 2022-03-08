# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ These Rspecs demonstrate Amber ...

describe 'Test Input Factory Test Suite' do
  before(:all) do
    @test = true
  end

  describe 'Test Suite' do
    it 'is true' do
      expect(@test).to be(true)
    end
  end
end

# -------------------------------------------------------------------------- }}}
