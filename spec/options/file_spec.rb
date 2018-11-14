require 'rspec'
require_relative '../../lib/amber/cli/options'
require_relative '../../lib/amber/cli/command_line_options'
# ------------------------------------------------------------------------------
# file options
# [-f | --file]
#
# [a.yaml | b.yaml
# ------------------------------------------------------------------------------
describe 'amber' do
  describe 'no -f' do
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
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
end
