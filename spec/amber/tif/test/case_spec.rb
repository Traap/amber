require 'rspec'
require 'amber'

# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber ...
# ------------------------------------------------------------------------------
describe 'Test Input Factory ' do

  before(:all) do
    @test = true
  end

  describe 'Test Case' do
    it 'is true' do
      expect(@test).to be(true)
    end
  end

end
