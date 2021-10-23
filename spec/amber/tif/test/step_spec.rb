# frozen_string_literal: true

require 'amber'

# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber ...
# ------------------------------------------------------------------------------
describe 'Test Input Factory Test Spec' do
  before(:all) do
    @test = true
  end

  describe 'Test Step' do
    it 'is true' do
      expect(@test).to be(true)
    end
  end
end
