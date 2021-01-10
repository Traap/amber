require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related to the 
# ${browser} keyword.
# ------------------------------------------------------------------------------
describe 'YAML Browser Substitutions' do

  describe 'Amber::Substitute.browser' do

    it "can substitute ${BROWSER} to #{Amber::Browser::DEFAULT}" do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, "${BROWSER}")).to eql(Amber::Browser::DEFAULT)
    end

    it "can substitute ${browser} to #{Amber::Browser::DEFAULT}" do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, "${browser}")).to eql(Amber::Browser::DEFAULT)
    end

    it 'can substitute ${BROWSER} to Brave' do
      ARGV.replace ['--browser', 'Brave']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${BROWSER}')).to eql('Brave')
    end

    it 'can substitute ${BROWSER} to Chrome' do
      ARGV.replace ['--browser', 'Chrome']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${BROWSER}')).to eql('Chrome')
    end

    it 'can substitute ${browser} to Edge' do
      ARGV.replace ['--browser', 'Edge']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${browser}')).to eql('Edge')
    end

    it 'can substitute ${BROWSER} to Firefox' do
      ARGV.replace ['--browser', 'Firefox']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${BROWSER}')).to eql('Firefox')
    end

    it 'can substitute ${browser} to IE' do
      ARGV.replace ['--browser', 'IE']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${browser}')).to eql('IE')
    end

    it 'can substitute ${BROWSER} to Opera' do
      ARGV.replace ['--browser', 'Opera']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${BROWSER}')).to eql('Opera')
    end

  end
end
