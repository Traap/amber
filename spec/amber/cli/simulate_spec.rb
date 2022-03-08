# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Simulate options.
#
# [-S | --simulate]
#
# -------------------------------------------------------------------------- }}}
# {{{ Simulate tests.

describe 'Amber CLO Simulate' do
  describe 'no -S' do
    it 'has not been used.' do
      options = Amber::Options.new
      expect(options.simulate?).to be(false)
    end
  end

  describe '-S' do
    it 'has been used from the command line.' do
      ARGV.replace ['-S']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.simulate?).to be(true)
    end
  end

  describe '--simulate' do
    it 'has been used from the command line.' do
      ARGV.replace ['--simulate']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.simulate?).to be(true)
    end
  end
end

# -------------------------------------------------------------------------- }}}
