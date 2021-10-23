# frozen_string_literal: true

require 'amber'
# ------------------------------------------------------------------------------
# Log Command options
# [--log-command | -L]
# ------------------------------------------------------------------------------
describe 'Amber CLO Logging' do
  describe '--log-command' do
    it 'was not used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.log_command?).to be(false)
    end
  end

  describe '--log-command' do
    it 'has been used from the command line.' do
      ARGV.replace ['--log-command']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.log_command?).to be(true)
    end
  end

  describe '-L' do
    it 'has been used from the command line.' do
      ARGV.replace ['-L']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.log_command?).to be(true)
    end
  end
end
