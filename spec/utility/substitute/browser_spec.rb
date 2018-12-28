require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related to the 
# ${browser} keyword.
# ------------------------------------------------------------------------------
describe 'YAML Browser Substitutions' do

  before(:all) do
    @options = Amber::Options.new
  end

  describe 'Amber::Substitute.browser' do

    it "can substitute ${BROWSER} to #{Amber::Browser::DEFAULT}" do
      expect(Amber::Substitute
        .browser(@options, "${BROWSER}")).to eql(Amber::Browser::DEFAULT)
    end

    it "can substitute ${browser} to #{Amber::Browser::DEFAULT}" do
      expect(Amber::Substitute
        .browser(@options, "${browser}")).to eql(Amber::Browser::DEFAULT)
    end

    it 'can substitute ${BROWSER} to Chrome' do
      @options.browser = 'Chrome' 
      expect(Amber::Substitute
        .browser(@options, '${BROWSER}')).to eql('Chrome')
    end

    it 'can substitute ${browser} to Edge' do
      @options.browser = 'Edge' 
      expect(Amber::Substitute
        .browser(@options, '${browser}')).to eql('Edge')
    end

    it 'can substitute ${BROWSER} to Firefox' do
      @options.browser = 'Firefox' 
      expect(Amber::Substitute
        .browser(@options, '${BROWSER}')).to eql('Firefox')
    end

    it 'can substitute ${browser} to IE' do
      @options.browser = 'IE'
      expect(Amber::Substitute
        .browser(@options, '${browser}')).to eql('IE')
    end

    it 'can substitute ${BROWSER} to Opera' do
      @options.browser = 'Opera' 
      expect(Amber::Substitute
        .browser(@options, '${BROWSER}')).to eql('Opera')
    end

  end
end
