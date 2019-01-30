require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# plan options
# [-p | --plan
#
# [foo | bar | baz]
#
# Fully-qualified extention at runtime:
#   factory/plan/foo/foo.yaml
#   factory/plan/bar/bar.yaml
#   factory/plan/baz/baz.yaml
# ------------------------------------------------------------------------------
describe 'Amber Plan' do

  before(:all) do
    @pwd = FileUtils.pwd()
  end

  describe 'no -p' do
    it 'has not been used.' do
      options = Amber::Options.new
      expect(options.filename).to eq([])
    end
  end

  describe '--plan=foo' do
    it 'has been used from the command line.' do
      ARGV.replace ['--plan=foo']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(["#{@pwd}/factory/plan/foo/foo.yaml"])
    end
  end

  describe '-pbar' do
    it 'has been used from the command line.' do
      ARGV.replace ['-pbar']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(["#{@pwd}/factory/plan/bar/bar.yaml"])
    end
  end

  describe '--plan baz' do
    it 'has been used from the command line.' do
      ARGV.replace ['--plan', 'baz']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(["#{@pwd}/factory/plan/baz/baz.yaml"])
    end
  end

  describe '-p foobar' do
    it 'has been used from the command line.' do
      ARGV.replace ['-p', 'foobar']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(["#{@pwd}/factory/plan/foobar/foobar.yaml"])
    end
  end
end
