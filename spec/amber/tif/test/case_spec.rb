# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ These Rspecs demonstrate Amber ...

describe 'Test Input Factory Test Case' do
  before(:all) do
    @test = true
  end

  describe 'Test Case' do
    it 'is true' do
      expect(@test).to be(true)
    end
  end
end

# -------------------------------------------------------------------------- }}}
