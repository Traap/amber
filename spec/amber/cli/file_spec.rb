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
# {{{ File tests short commands.

describe 'Amber CLO File Short' do
  it 'no -f has not been used.' do
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.filename).to eq(nil)
    expect(options.files).to    eq(nil)
  end

  it '-fa.yaml has been used from the command line.' do
    ARGV.replace ['-fa.yaml']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.filename).to eq(['a.yaml'])
    expect(options.files).to    eq(['a.yaml'])
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ File tests long commands.

describe 'Amber CLO File Long' do
  it '--file has not been used.' do
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.filename).to eq(nil)
    expect(options.files).to    eq(nil)
  end

  it '--file=b.yaml has been used from the command line.' do
    ARGV.replace ['--file=b.yaml']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.filename).to eq(['b.yaml'])
    expect(options.files).to    eq(['b.yaml'])
  end

  it '--file c.yaml has been used from the command line.' do
    ARGV.replace ['--file', 'c.yaml']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.filename).to eq(['c.yaml'])
    expect(options.files).to    eq(['c.yaml'])
  end
end

# -------------------------------------------------------------------------- }}}
