require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# default options
# ------------------------------------------------------------------------------
describe 'amber' do
  describe 'default options' do
    it 'have been provided.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.browser).to eq('None')
      expect(options.dryrun).to be(true)
      expect(options.environment).to be(false)
      expect(options.filename).to eq([])
      expect(options.language).to eq('zz')
      expect(options.parser).to eq(nil)
      expect(options.obliterate).to be(false)
      expect(options.simulate).to be(false)
      expect(options.verbose).to be(false)
      expect(options.writer).to eq(Amber::Writer::DEFAULT)
      expect(options.has_browser?).to be(false)
      expect(options.has_language?).to be(false)
    end
  end
end
