require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities relative to the
# ${home} keyword.
# ------------------------------------------------------------------------------
describe 'YAML Home Substitutions' do

  before(:all) do
    @options = Amber::Options.new
  end

  describe 'Amber::Substitute.home' do

    it 'can substitute ${home} to ~' do
      expect(Amber::Substitute.home("${home}")).to eql('~')
    end

    it 'can substitute ${HOME} to ~' do
      expect(Amber::Substitute.home("${HOME}")).to eql('~')
    end

    it 'can substitute ${home} and ${HOME} to ~ and ~' do
      expect(Amber::Substitute.home("${home} and ${HOME}")).to eql('~ and ~')
    end

  end
end
