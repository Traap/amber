# frozen_string_literal: true

require 'amber'
# ------------------------------------------------------------------------------
# browser options
# [-b | --browser]
#
# [None | Chrome | Edge | Firefox | IE | Opera]
# ------------------------------------------------------------------------------
describe 'Amber CLO Browser' do
  describe '--browser=Chrome' do
    it 'has been used from the command line.' do
      ARGV.replace ['--browser=Chrome']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('Chrome')
    end
  end

  describe '--browser Chrome' do
    it 'has been used from the command line.' do
      ARGV.replace ['--browser', 'Chrome']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('Chrome')
    end
  end

  describe '-bChrome' do
    it 'has been used from the command line.' do
      ARGV.replace ['-b', 'Chrome']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('Chrome')
    end
  end

  describe '--browser=Firefox' do
    it 'has been used from the command line.' do
      ARGV.replace ['--browser=Firefox']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('Firefox')
    end
  end

  describe '--browser Firefox' do
    it 'has been used from the command line.' do
      ARGV.replace ['--browser', 'Firefox']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('Firefox')
    end
  end

  describe '-bFirefox' do
    it 'has been used from the command line.' do
      ARGV.replace ['-b', 'Firefox']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('Firefox')
    end
  end

  describe '--browser=IE' do
    it 'has been used from the command line.' do
      ARGV.replace ['--browser=IE']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('IE')
    end
  end

  describe '--browser IE' do
    it 'has been used from the command line.' do
      ARGV.replace ['--browser', 'IE']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('IE')
    end
  end

  describe '-bIE' do
    it 'has been used from the command line.' do
      ARGV.replace ['-b', 'IE']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('IE')
    end
  end

  describe '--browser=Opra' do
    it 'has been used from the command line.' do
      ARGV.replace ['--browser=Opera']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('Opera')
    end
  end

  describe '--browser Opra' do
    it 'has been used from the command line.' do
      ARGV.replace ['--browser', 'Opera']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('Opera')
    end
  end

  describe '-bOpra' do
    it 'has been used from the command line.' do
      ARGV.replace ['-b', 'Opera']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('Opera')
    end
  end
end
