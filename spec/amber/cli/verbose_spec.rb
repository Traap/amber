# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Verbose options.
# [-v | --verbose]
#
# -------------------------------------------------------------------------- }}}
# {{{ Verbose tests.

describe 'Amber CLO Verbose' do
  describe 'no -v' do
    it 'has not been used.' do
      options = Amber::Options.new
      expect(options.verbose?).to be(false)
    end
  end

  describe '-v' do
    it 'has been used from the command line.' do
      ARGV.replace ['-v']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.verbose?).to be(true)
    end
  end

  describe '--verbose' do
    it 'has been used from the command line.' do
      ARGV.replace ['--verbose']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.verbose?).to be(true)
    end
  end
end

# -------------------------------------------------------------------------- }}}
