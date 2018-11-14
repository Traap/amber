require 'rspec'
require_relative '../../lib/amber/cli/options'
require_relative '../../lib/amber/cli/command_line_options'
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
describe 'amber' do
  describe 'no -c' do
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq([])
    end
  end

  describe '--case=foo' do
    it 'has been used from the command line.' do
      ARGV.replace ['--case=foo']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/case/foo/foo.yaml'])
    end
  end

  describe '-cbar' do
    it 'has been used from the command line.' do
      ARGV.replace ['-cbar']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/case/bar/bar.yaml'])
    end
  end

  describe '--case baz' do
    it 'has been used from the command line.' do
      ARGV.replace ['--case', 'baz']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/case/baz/baz.yaml'])
    end
  end

  describe '-c foobar' do
    it 'has been used from the command line.' do
      ARGV.replace ['-c', 'foobar']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['factory/case/foobar/foobar.yaml'])
    end
  end
end
