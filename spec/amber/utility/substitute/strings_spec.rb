require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related to the 
# string function. 
# ------------------------------------------------------------------------------
describe 'YAML Strings Substitutions' do

  before(:all) do
    @filename = 'foo/bar/baz/baz.yaml'
    @options = Amber::Options.new
    @options.browser = 'Opera'
    @options.language = 'Svenska'
  end

  describe 'Amber::Substitute.strings' do

    it 'can substitute ${BROWSER} to Opera' do
      expect(Amber::Substitute
        .strings(@filename, @options, '${BROWSER}')).to eql('Opera')
    end
  
    it 'can substitute ${browser} ${file} ${language} and ${language-code} to Opera baz Svenska and sv' do
      expect(Amber::Substitute
        .strings(@filename, @options, '${browser} ${file} ${language} and ${language-code}')).to eql('Opera baz Svenska and sv')
    end
    it 'can substitute ${language-code}${file}${language}${browser} to svbazSvenskaOpera' do
      expect(Amber::Substitute
        .strings(@filename, @options, '${language-code}${file}${language}${browser}')).to eql('svbazSvenskaOpera')
    end


  end
end
