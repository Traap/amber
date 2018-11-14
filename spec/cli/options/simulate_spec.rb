require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# simulate options
# [-S | --simulate]
# ------------------------------------------------------------------------------
describe 'amber' do
  describe 'no -S' do
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.simulate).to be(false)
    end
  end

  describe '-S' do
    it 'has been used from the command line.' do
      ARGV.replace ['-S']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.simulate).to be(true)
    end
  end

  describe '--simulate' do
    it 'has been used from the command line.' do
      ARGV.replace ['--simulate']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.simulate).to be(true)
    end
  end
end
