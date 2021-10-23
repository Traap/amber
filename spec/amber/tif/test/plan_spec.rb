# frozen_string_literal: true

require 'amber'

# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber ...
# ------------------------------------------------------------------------------
describe 'Test Input Factory Test Plan' do
  before(:all) do
    @test = true
  end

  describe 'Test Plan' do
    it 'is true' do
      expect(@test).to be(true)
    end
  end
end
