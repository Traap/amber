require 'rspec'
require_relative '../lib/amber/options'
# ------------------------------------------------------------------------------
# version options
# [--version]
# ------------------------------------------------------------------------------
describe 'amber' do
  describe 'no --version' do 
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.verbose).to be(false)
    end
  end

  describe '--version' do 
    it 'has been used from the command line.' do
      ARGV.replace ['--version']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.version).to eql('1.2.174')
      expect(options.version).to eql(Amber::VERSION)
    end
  end
end
