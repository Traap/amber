# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Browser keyword substitution.
#
# These Rspecs demonstrate Amber substitution capabilities related to the
# ${browser} keyword.
#
# -------------------------------------------------------------------------- }}}
# {{{ Browser keyword substitution: Default.

describe 'YAML Browser Substitutions' do
  describe 'Amber::Substitute.browser' do
    it "can substitute ${BROWSER} to #{Amber::Browser::DEFAULT}" do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${BROWSER}')).to eql(Amber::Browser::DEFAULT)
    end

    it "can substitute ${browser} to #{Amber::Browser::DEFAULT}" do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${browser}')).to eql(Amber::Browser::DEFAULT)
    end
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Browser keyword substitution: Brave.

describe 'YAML Browser Substitutions' do
  describe 'Amber::Substitute.browser' do
    it 'can substitute ${BROWSER} to Brave' do
      ARGV.replace ['--browser', 'Brave']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${BROWSER}')).to eql('Brave')
    end
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Browser keyword substitution: Chrome.

describe 'YAML Browser Substitutions' do
  describe 'Amber::Substitute.browser' do
    it 'can substitute ${BROWSER} to Chrome' do
      ARGV.replace ['--browser', 'Chrome']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${BROWSER}')).to eql('Chrome')
    end
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Browser keyword substitution: Edge.

describe 'YAML Browser Substitutions' do
  describe 'Amber::Substitute.browser' do
    it 'can substitute ${browser} to Edge' do
      ARGV.replace ['--browser', 'Edge']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${browser}')).to eql('Edge')
    end
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Browser keyword substitution: Firefox.

describe 'YAML Browser Substitutions' do
  describe 'Amber::Substitute.browser' do
    it 'can substitute ${BROWSER} to Firefox' do
      ARGV.replace ['--browser', 'Firefox']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${BROWSER}')).to eql('Firefox')
    end
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Browser keyword substitution: IE.

describe 'YAML Browser Substitutions' do
  describe 'Amber::Substitute.browser' do
    it 'can substitute ${browser} to IE' do
      ARGV.replace ['--browser', 'IE']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${browser}')).to eql('IE')
    end
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Browser keyword substitution: Opera.

describe 'YAML Browser Substitutions' do
  describe 'Amber::Substitute.browser' do
    it 'can substitute ${BROWSER} to Opera' do
      ARGV.replace ['--browser', 'Opera']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(Amber::Substitute
        .browser(options, '${BROWSER}')).to eql('Opera')
    end
  end
end

# -------------------------------------------------------------------------- }}}
