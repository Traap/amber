require 'rspec'
require_relative '../lib/amber/options'

describe 'amber' do
  # ----------------------------------------------------------------------------
  # file options
  # [-f | --file]
  # 
  # [a.yaml | b.yaml
  # ----------------------------------------------------------------------------
  describe 'no -f' do 
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV) 
      expect(options.filename).to eq([])
    end
  end

  describe '-f' do 
    it 'has been used from the command line.' do
      ARGV.replace ['-f a.yaml']
      options = Amber::CommandLineOptions.parse(ARGV) 
      expect(options.filename).to eq([' a.yaml'])
    end
  end

  describe '-f' do 
    it 'has been used from the command line.' do
      ARGV.replace ['-f b.yaml']
      options = Amber::CommandLineOptions.parse(ARGV) 
      expect(options.filename).to eq([' b.yaml'])
    end
  end

end
