require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# These RSpecs demonstrate Amber substitution capabilities.
# ------------------------------------------------------------------------------
describe 'YAML file Substitution' do

  # ----------------------------------------------------------------------------
  describe 'File name' do

    it 'can substitute ${file} with base filename from foo/bar/baz.yaml' do
      expect(Amber::Substitute
        .file('foo/bar/baz.txt', '${file}')).to eql('baz')
    end
  end

  # ----------------------------------------------------------------------------
  describe 'Home path' do

    it 'can substitute ${home} to ~' do
      expect(Amber::Substitute.home("${home}")).to eql('~')
    end

    it 'can substitute ${home} to ~' do
      expect(Amber::Substitute.home("${home}")).to eql('~')
    end

    it 'can substitute ${home} and ${HOME} to ~ and ~' do
      expect(Amber::Substitute.home("${home} and ${HOME}")).to eql('~ and ~')
    end

  end
 
  # ----------------------------------------------------------------------------
  describe 'Expand path' do
    
    it "can expand ~ to #{Dir.home}" do
      expect(Amber::Substitute.expand_path('~').strip).to eql(Dir.home)
    end

    it "can expand ~ and ~ to #{Dir.home} and #{Dir.home}" do
      expect(Amber::Substitute
        .expand_path('~ and ~').strip).to eql("#{Dir.home} and #{Dir.home}")
    end

  end
  # ----------------------------------------------------------------------------

end

