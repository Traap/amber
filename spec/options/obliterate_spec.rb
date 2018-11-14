require 'rspec'
require_relative '../../lib/amber/cli/options'
require_relative '../../lib/amber/cli/command_line_options'
# ------------------------------------------------------------------------------
# obliterate options
# [-O | --obliterate]
# ------------------------------------------------------------------------------
describe 'amber' do
  describe 'no -O' do
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.obliterate).to be(false)
    end
  end

  describe '-O' do
    it 'has been used from the command line.' do
      ARGV.replace ['-O']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.obliterate).to be(true)
    end
  end

  describe '--obliterate' do
    it 'has been used from the command line.' do
      ARGV.replace ['--obliterate']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.obliterate).to be(true)
    end
  end
end
