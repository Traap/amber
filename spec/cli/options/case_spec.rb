require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# case options
# [-c | --case
#
# [foo | bar | baz]
#
# Fully-qualified extention at runtime:
#   factory/case/foo/foo.yaml
#   factory/case/bar/bar.yaml
#   factory/case/baz/baz.yaml
# ------------------------------------------------------------------------------
describe 'Amber Case' do

  describe '--case=foo' do
    it 'has been used from the command line.' do
      ARGV.replace ['--case=foo']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/case/foo/foo.yaml'])
    end
  end

  describe '--case foo' do
    it 'has been used from the command line.' do
      ARGV.replace ['--case','foo']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/case/foo/foo.yaml'])
    end
  end

  describe '-cfoo' do
    it 'has been used from the command line.' do
      ARGV.replace ['-c', 'foo']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/case/foo/foo.yaml'])
    end
  end

  describe '--case=foo/baz' do
    it 'has been used from the command line.' do
      ARGV.replace ['--case=foo/baz']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/case/foo/baz/baz.yaml'])
    end
  end

  describe '--case foo/baz' do
    it 'has been used from the command line.' do
      ARGV.replace ['--case', 'foo/baz']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/case/foo/baz/baz.yaml'])
    end
  end

  describe '--coo/baz' do
    it 'has been used from the command line.' do
      ARGV.replace ['-c', 'foo/baz']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/case/foo/baz/baz.yaml'])
    end
  end

end
