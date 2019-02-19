require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# default options
# ------------------------------------------------------------------------------
describe 'Amber CLO Defaults' do

  before(:all) do
    @options = Amber::Options.new
  end

  describe 'for browser' do
    it 'has been provided.' do
      expect(@options.browser).to eq('None')
    end
  end

  describe 'for dryrun' do
    it 'has been provided.' do
      expect(@options.dryrun).to be(true)
    end
  end

  describe 'for environment' do
    it 'has been provided.' do
      expect(@options.environment).to be(false) end end

  describe 'for filename' do
    it 'has been provided.' do
      expect(@options.filename).to eq([])
    end
  end

  describe 'for language' do
    it 'has been provided.' do
      expect(@options.language).to eq(Amber::Language::DEFAULT)
    end
  end

  describe 'for parser' do
    it 'has been provided.' do
      expect(@options.parser).to eq(nil)
    end
  end

  describe 'for obliterate' do
    it 'has been provided.' do
      expect(@options.obliterate).to be(false)
    end
  end

  describe 'for simulate' do
    it 'has been provided.' do
      expect(@options.simulate).to be(false)
    end
  end

  describe 'for verbose' do
    it 'has been provided.' do
      expect(@options.verbose).to be(false)
    end
  end

  describe 'for writer' do
    it 'has been provided.' do
      expect(@options.writer).to eq(Amber::Writer::DEFAULT)
    end
  end

  describe 'for okay_to_run?' do
    it 'has been provided.' do
      expect(@options.okay_to_run?).to be(false)
    end
  end

  describe 'for okay_to_echo_env?' do
    it 'has been provided.' do
      expect(@options.okay_to_echo_env?).to be(false)
    end
  end

  describe 'for okay_to_obliterate?' do
    it 'has been provided.' do
      expect(@options.okay_to_obliterate?).to be(false)
    end
  end

  describe 'for has_browser?' do
    it 'has been provided.' do
      expect(@options.has_browser?).to be(false)
    end
  end

  describe 'for has_language?' do
    it 'has been provided.' do
      expect(@options.has_language?).to be(false)
    end
  end

end
