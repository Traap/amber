require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# environments options
# [-e | --environment]
# ------------------------------------------------------------------------------
describe 'Amber CLO Environment' do

  describe 'no -e' do
    it 'has not been used.' do
      options = Amber::Options.new
      expect(options.environment).to be(false)
    end
  end

  describe '-e' do
    it 'has been used from the command line.' do
      ARGV.replace ['-e']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.environment).to be(true)
    end
  end

  describe '--environment' do
    it 'has been used from the command line.' do
      ARGV.replace ['--environment']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.environment).to be(true)
    end
  end
end
