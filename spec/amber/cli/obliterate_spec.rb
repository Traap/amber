# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Obliterate options.
#
# [-O | --obliterate]
#
# -------------------------------------------------------------------------- }}}
# {{{ Obliterate tests.

describe 'Amber CLO Obliterate' do
  describe 'no -O' do
    it 'has not been used.' do
      options = Amber::Options.new
      expect(options.obliterate?).to be(false)
    end
  end

  describe '-O' do
    it 'has been used from the command line.' do
      ARGV.replace ['-O']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.obliterate?).to be(true)
    end
  end

  describe '--obliterate' do
    it 'has been used from the command line.' do
      ARGV.replace ['--obliterate']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.obliterate?).to be(true)
    end
  end
end

# -------------------------------------------------------------------------- }}}
