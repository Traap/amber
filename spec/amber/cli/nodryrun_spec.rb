# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ NoDryRun options.
#
# nodryrun options
# [-v | --verbose]
#
# -------------------------------------------------------------------------- }}}
# {{{ NoDryRun tests.

describe 'Amber CLO NoDryRun' do
  describe 'no -n' do
    it 'has not been used.' do
      options = Amber::Options.new
      expect(options.dryrun?).to be(true)
    end
  end

  describe '-n' do
    it 'has been used from the command line.' do
      ARGV.replace ['-n']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.dryrun?).to be(false)
    end
  end

  describe '--nodryrun' do
    it 'has been used from the command line.' do
      ARGV.replace ['--nodryrun']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.dryrun?).to be(false)
    end
  end
end

# -------------------------------------------------------------------------- }}}
