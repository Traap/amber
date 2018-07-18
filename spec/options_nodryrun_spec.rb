require 'rspec'
require_relative '../lib/amber/options'
# ------------------------------------------------------------------------------
# nodryrun options
# [-v | --verbose]
# ------------------------------------------------------------------------------
describe 'amber' do
  describe 'no -n' do 
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV) 
      expect(options.dryrun).to be(true)
    end
  end

  describe '-n' do 
    it 'has been used from the command line.' do
      ARGV.replace ['-n']
      options = Amber::CommandLineOptions.parse(ARGV) 
      expect(options.dryrun).to be(false)
    end
  end

  describe '--nodryrun' do 
    it 'has been used from the command line.' do
      ARGV.replace ['--nodryrun']
      options = Amber::CommandLineOptions.parse(ARGV) 
      expect(options.dryrun).to be(false)
    end
  end
end
