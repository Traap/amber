# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Writer options.
#
# [-w | --writer]
#
# [Ascii | LaTeX]
#
# -------------------------------------------------------------------------- }}}
# {{{ Writer tests short commands.

describe 'Amber CLO Writer Short' do
  it '-w  has not been used.' do
    options = Amber::Options.new
    expect(options.writer).to eq('LaTeX')
  end

  it '-wAscii has been used from the command line.' do
    ARGV.replace ['-wAscii']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.writer).to eq('Ascii')
  end

  it '-w Ascii has been used from the command line.' do
    ARGV.replace ['-w', 'Ascii']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.writer).to eq('Ascii')
  end

  it '-wLaTeX has been used from the command line.' do
    ARGV.replace ['-wLaTeX']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.writer).to eq('LaTeX')
  end

  it '-w LaTeX has been used from the command line.' do
    ARGV.replace ['-w', 'LaTeX']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.writer).to eq('LaTeX')
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Writer tests long commands.

describe 'Amber CLO Writer Long' do
  it '--writer has not been used.' do
    options = Amber::Options.new
    expect(options.writer).to eq('LaTeX')
  end

  it '--writer=Ascii has been used from the command line.' do
    ARGV.replace ['--writer=Ascii']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.writer).to eq('Ascii')
  end

  it '--writer Ascii has been used from the command line.' do
    ARGV.replace ['--writer', 'Ascii']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.writer).to eq('Ascii')
  end

  it '--writer=LaTeX has been used from the command line.' do
    ARGV.replace ['--writer=LaTeX']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.writer).to eq('LaTeX')
  end

  it '--writer LaTeX has been used from the command line.' do
    ARGV.replace ['--writer', 'LaTeX']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.writer).to eq('LaTeX')
  end
end

# -------------------------------------------------------------------------- }}}
