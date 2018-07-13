require 'rspec'
require_relative '../lib/amber/options'

describe 'amber' do
  # ----------------------------------------------------------------------------
  # default options
  # ----------------------------------------------------------------------------
  describe 'default options' do
    it 'have been provided.' do
      options = Amber::CommandLineOptions.parse(ARGV) 
      expect(options.browser).to eq(nil)
      expect(options.dryrun).to be(true)
      expect(options.environment).to be(false)
      expect(options.filename).to eq([])
      expect(options.language).to eq(nil)
      expect(options.parser).to eq(nil)
      expect(options.obliterate).to be(false)
      expect(options.simulate).to be(false)
      expect(options.verbose).to be(false)
      expect(options.writer).to eq(Amber::Writer::DEFAULT)
    end
  end
end
