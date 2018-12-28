require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related to the 
# ${language-code} keyword.
# ------------------------------------------------------------------------------
describe 'YAML Language Code Substitutions' do

  before(:all) do
    @options = Amber::Options.new
  end

  describe 'Amber::Substitute.langage_code' do

    it "can substitute ${LANGUAGE-CODE} to #{Amber::Language::DEFAULT}" do
      @options.language = 'n/a'
      expect(Amber::Substitute
        .language_code(@options, '${LANGUAGE-CODE}')).to eql('zz')
    end

    it 'can substitute ${langauge-code} to cs' do
      @options.language = 'Czech'
      expect(Amber::Substitute
        .language_code(@options, '${language-code}')).to eql('cs')
    end

    it 'can substitute ${LANGAUGE-CODE} to da' do
      @options.language = 'Dansk'
      expect(Amber::Substitute
        .language_code(@options, '${LANGUAGE-CODE}')).to eql('da')
    end

    it 'can substitute ${langauge-CODE} to de' do
      @options.language = 'Deutsch'
      expect(Amber::Substitute
        .language_code(@options, '${language-code}')).to eql('de')
    end

    it 'can substitute ${LANGAUGE-CODE} to en' do
      @options.language = 'English'
      expect(Amber::Substitute
        .language_code(@options, '${LANGUAGE-CODE}')).to eql('en')
    end

    it 'can substitute ${langauge-code} to es' do
      @options.language = 'Espanol'
      expect(Amber::Substitute
        .language_code(@options, '${language-code}')).to eql('es')
    end

    it 'can substitute ${LANGAUGE-CODE} to fr-ca' do
      @options.language = 'CA French - Canadian'
      expect(Amber::Substitute
        .language_code(@options, '${LANGUAGE-CODE}')).to eql('fr-ca')
    end

    it 'can substitute ${langauge-code} to fr-eu' do
      @options.language = 'EU French - European'
      expect(Amber::Substitute
        .language_code(@options, '${language-code}')).to eql('fr-eu')
    end

    it 'can substitute ${LANGAUGE-CODE} to it' do
      @options.language = 'Italiano'
      expect(Amber::Substitute
        .language_code(@options, '${LANGUAGE-CODE}')).to eql('it')
    end

    it 'can substitute ${langauge-code} to ne' do
      @options.language = 'Nederlands'
      expect(Amber::Substitute
        .language_code(@options, '${language-code}')).to eql('ne')
    end

    it 'can substitute ${LANGAUGE-CODE} to no' do
      @options.language = 'Norsk'
      expect(Amber::Substitute
        .language_code(@options, '${LANGUAGE-CODE}')).to eql('no')
    end

    it 'can substitute ${langauge-code} to pl' do
      @options.language = 'Polish'
      expect(Amber::Substitute
        .language_code(@options, '${language-code}')).to eql('pl')
    end

    it 'can substitute ${LANGAUGE-CODE} to ro' do
      @options.language = 'Romanian'
      expect(Amber::Substitute
        .language_code(@options, '${LANGUAGE-CODE}')).to eql('ro')
    end

    it 'can substitute ${langauge-code} to sv' do
      @options.language = 'Svenska'
      expect(Amber::Substitute
        .language_code(@options, '${language-code}')).to eql('sv')
    end

  end
end
