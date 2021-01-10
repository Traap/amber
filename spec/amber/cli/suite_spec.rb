require 'amber'
# ------------------------------------------------------------------------------
# suite options
# [-s | --suite
#
# [foo | bar | baz]
#
# Fully-qualified extention at runtime:
#   factory/suite/foo/foo.yaml
#   factory/suite/bar/bar.yaml
#   factory/suite/baz/baz.yaml
# ------------------------------------------------------------------------------
describe 'Amber CLO Suite' do

  describe 'no -s' do
    it 'has not been used.' do
      options = Amber::Options.new
      expect(options.filename).to eq([])
    end
  end

  describe '--suite=foo' do
    it 'has been used from the command line.' do
      ARGV.replace ['--suite=foo']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/suite/foo/foo.yaml'])
    end
  end

  describe '-sbar' do
    it 'has been used from the command line.' do
      ARGV.replace ['-sbar']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/suite/bar/bar.yaml'])
    end
  end

  describe '--suite baz' do
    it 'has been used from the command line.' do
      ARGV.replace ['--suite', 'baz']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/suite/baz/baz.yaml'])
    end
  end

  describe '-s foobar' do
    it 'has been used from the command line.' do
      ARGV.replace ['-s', 'foobar']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/suite/foobar/foobar.yaml'])
    end
  end

end
