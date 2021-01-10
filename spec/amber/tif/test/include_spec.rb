require 'amber'

# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber ...
# ------------------------------------------------------------------------------
describe 'Test Input Factory Test Include' do

  before(:all) do
    @test = true
  end

  describe 'Test Include' do
    it 'is true' do
      expect(@test).to be(true)
    end
  end

end
