require 'amber'
# ------------------------------------------------------------------------------
# Requirement options
# [--requirement | -r]
# ------------------------------------------------------------------------------
describe 'Amber CLO Requirements' do

  describe '--log-requirement' do
    it 'was not used.' do
      ARGV.replace ['--nodryrun']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.log_requirement?).to be(false)
    end
  end

  describe '--log-requirement' do
    it 'has been used from the command line.' do
      ARGV.replace ['--log-requirement']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.log_requirement?).to be(true) 
    end
  end

  describe '-r' do
    it 'has been used from the command line.' do
      ARGV.replace ['-r']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.log_requirement?).to be(true)
    end
  end

end
