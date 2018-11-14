require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# browser options
# [-b | --browser]
#
# [None | Chrome | Edge | Firefox | IE | Opera]
# ------------------------------------------------------------------------------
describe 'amber' do
  # describe 'no -b' do
  #   it 'has not been used.' do
  #     options = Amber::CommandLineOptions.parse(ARGV)
  #     expect(options.browser).to eq('None')
  #   end
  # end

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
end
