require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# file options
# [-f | --file]
#
# [a.yaml | b.yaml
# ------------------------------------------------------------------------------
describe 'Amber File' do

  describe 'no -f' do
    it 'has not been used.' do
      options = Amber::Options.new 
      expect(options.filename).to eq([])
    end
  end

  describe '-fa.yaml' do
    it 'has been used from the command line.' do
      ARGV.replace ['-fa.yaml']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['a.yaml'])
    end
  end

  describe '--file=b.yaml' do
    it 'has been used from the command line.' do
      ARGV.replace ['--file=b.yaml']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['b.yaml'])
    end
  end

  describe '--file c.yaml' do
    it 'has been used from the command line.' do
      ARGV.replace ['--file', 'c.yaml']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['c.yaml'])
    end
  end

end
