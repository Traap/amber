# frozen_string_literal: true

require 'amber'
# ------------------------------------------------------------------------------
# writer options
# [-w | --writer]
#
# [Ascii | LaTeX]
# ------------------------------------------------------------------------------
describe 'Amber CLO Writer' do
  describe 'no -w' do
    it 'has not been used.' do
      options = Amber::Options.new
      expect(options.writer).to eq('LaTeX')
    end
  end

  describe '--writer=Ascii' do
    it 'has been used from the command line.' do
      ARGV.replace ['--writer=Ascii']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('Ascii')
    end
  end

  describe '--writer Ascii' do
    it 'has been used from the command line.' do
      ARGV.replace ['--writer', 'Ascii']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('Ascii')
    end
  end

  describe '-wAscii' do
    it 'has been used from the command line.' do
      ARGV.replace ['-wAscii']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('Ascii')
    end
  end

  describe '-w Ascii' do
    it 'has been used from the command line.' do
      ARGV.replace ['-w', 'Ascii']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('Ascii')
    end
  end

  describe '--writer=LaTeX' do
    it 'has been used from the command line.' do
      ARGV.replace ['--writer=LaTeX']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('LaTeX')
    end
  end

  describe '--writer LaTeX' do
    it 'has been used from the command line.' do
      ARGV.replace ['--writer', 'LaTeX']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('LaTeX')
    end
  end

  describe '-wLaTeX' do
    it 'has been used from the command line.' do
      ARGV.replace ['-wLaTeX']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('LaTeX')
    end
  end

  describe '-w LaTeX' do
    it 'has been used from the command line.' do
      ARGV.replace ['-w', 'LaTeX']
      options = Amber::CommandLineOptions.parse(ARGV)
      expect(options.writer).to eq('LaTeX')
    end
  end
end
