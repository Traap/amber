require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# writer options
# [-w | --writer]
#
# [Ascii | LaTeX]
# ------------------------------------------------------------------------------
describe 'amber' do
  describe 'no -w' do
    it 'has not been used.' do
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('LaTeX')
    end
  end

  describe '--writer Ascii' do
    it 'has been used from the command line.' do
      ARGV.replace ['--writer=Ascii']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('Ascii')
    end
  end

  describe '--writer LaTeX' do
    it 'has been used from the command line.' do
      ARGV.replace ['--writer=LaTeX']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('LaTeX')
    end
  end
end
