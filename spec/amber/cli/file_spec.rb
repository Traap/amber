# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ File options.
#
# file options
# [-f | --file]
#
# [a.yaml | b.yaml
#
# -------------------------------------------------------------------------- }}}
# {{{ File tests.

describe 'Amber CLO File' do
  describe 'no -f' do
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(nil)
      expect(options.files).to    eq(nil)
    end
  end

  describe '-fa.yaml' do
    it 'has been used from the command line.' do
      ARGV.replace ['-fa.yaml']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['a.yaml'])
      expect(options.files).to    eq(['a.yaml'])
    end
  end

  describe '--file=b.yaml' do
    it 'has been used from the command line.' do
      ARGV.replace ['--file=b.yaml']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['b.yaml'])
      expect(options.files).to    eq(['b.yaml'])
    end
  end

  describe '--file c.yaml' do
    it 'has been used from the command line.' do
      ARGV.replace ['--file', 'c.yaml']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.filename).to eq(['c.yaml'])
      expect(options.files).to    eq(['c.yaml'])
    end
  end
end

# -------------------------------------------------------------------------- }}}
