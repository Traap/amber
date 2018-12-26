require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related to the 
# ${language} keyword.
# ------------------------------------------------------------------------------
describe 'YAML Language Substitutions' do

  before(:all) do
    @options = Amber::Options.new
  end

  describe 'Amber::Substitute.language' do

    it 'can substitute ${LANGUAGE} to n/a' do
      @options.language = 'n/a'
      expect(Amber::Substitute
        .language(@options, '${LANGUAGE}')).to eql('n/a')
    end

    it 'can substitute ${langauge} to Chez' do
      @options.language = 'Chez'
      expect(Amber::Substitute
        .language(@options, '${language}')).to eql('Chez')
    end

    it 'can substitute ${LANGAUGE} to Dansk' do
      @options.language = 'Dansk'
      expect(Amber::Substitute
        .language(@options, '${LANGUAGE}')).to eql('Dansk')
    end

    it 'can substitute ${langauge} to Deutsch' do
      @options.language = 'Deutsch'
      expect(Amber::Substitute
        .language(@options, '${language}')).to eql('Deutsch')
    end

    it 'can substitute ${LANGAUGE} to English' do
      @options.language = 'English'
      expect(Amber::Substitute
        .language(@options, '${LANGUAGE}')).to eql('English')
    end

    it 'can substitute ${langauge} to Espanol' do
      @options.language = 'Espanol'
      expect(Amber::Substitute
        .language(@options, '${language}')).to eql('Espanol')
    end

    it 'can substitute ${LANGAUGE} to CA French - Canadian' do
      @options.language = 'CA French - Canadian'
      expect(Amber::Substitute
        .language(@options, '${LANGUAGE}')).to eql('CA French - Canadian')
    end

    it 'can substitute ${langauge} to EU French - European' do
      @options.language = 'EU French - European'
      expect(Amber::Substitute
        .language(@options, '${language}')).to eql('EU French - European')
    end

    it 'can substitute ${LANGAUGE} to Italiano' do
      @options.language = 'Italiano'
      expect(Amber::Substitute
        .language(@options, '${LANGUAGE}')).to eql('Italiano')
    end

    it 'can substitute ${langauge} to Nederlands' do
      @options.language = 'Nederlands'
      expect(Amber::Substitute
        .language(@options, '${language}')).to eql('Nederlands')
    end

    it 'can substitute ${LANGAUGE} to Norsk' do
      @options.language = 'Norsk'
      expect(Amber::Substitute
        .language(@options, '${LANGUAGE}')).to eql('Norsk')
    end

    it 'can substitute ${langauge} to Polish' do
      @options.language = 'Polish'
      expect(Amber::Substitute
        .language(@options, '${language}')).to eql('Polish')
    end

    it 'can substitute ${LANGAUGE} to Romanian' do
      @options.language = 'Romanian'
      expect(Amber::Substitute
        .language(@options, '${LANGUAGE}')).to eql('Romanian')
    end

    it 'can substitute ${langauge} to Svenska' do
      @options.language = 'Svenska'
      expect(Amber::Substitute
        .language(@options, '${language}')).to eql('Svenska')
    end

  end
end
