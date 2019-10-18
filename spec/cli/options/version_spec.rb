# frozen_string_literal: true

require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# version options
# [--version]
# ------------------------------------------------------------------------------
describe 'Amber CLO Version' do
  before(:all) do
    @version = '1.5.285'
  end

  describe 'no --version' do
    it "was not used. However the version number must match #{Amber::VERSION}" do
      expect(Amber::VERSION).to eql(@version)
    end
  end

  describe '--version' do
    it 'has been used from the command line.' do
      ARGV.replace ['--version']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.verbose).to be(false)
    end
  end

  describe 'Version' do
    it 'has a version number' do
      expect(Amber::VERSION).not_to be nil
    end

    it "version number must match #{Amber::VERSION}" do
      expect(Amber::VERSION).to eql(@version)
    end
  end
end
