# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Case options.
#
# [-c | --case
#
# [foo | bar | baz]
#
# Fully-qualified extention at runtime:
#   factory/case/foo/foo.yaml
#   factory/case/bar/bar.yaml
#   factory/case/baz/baz.yaml
#
# -------------------------------------------------------------------------- }}}
# {{{ Case test short commands.

describe 'Amber CLO Case' do
  it '-c has not been used.' do
    options = Amber::Options.new
    expect(options.test_case).to eq(nil)
    expect(options.files).to     eq(nil)
  end

  it '-cbar has been used from the command line.' do
    ARGV.replace ['-cbar']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_case).to eq(['factory/case/bar/bar.yaml'])
    expect(options.files).to     eq(['factory/case/bar/bar.yaml'])
  end

  it '-c foobar has been used from the command line.' do
    ARGV.replace ['-c', 'foobar']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_case).to eq(['factory/case/foobar/foobar.yaml'])
    expect(options.files).to     eq(['factory/case/foobar/foobar.yaml'])
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Case test long commands.

describe 'Amber CLO Case' do
  it '--case has not been used.' do
    options = Amber::Options.new
    expect(options.test_case).to eq(nil)
    expect(options.files).to     eq(nil)
  end

  it '--case=foo has been used from the command line.' do
    ARGV.replace ['--case=foo']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_case).to eq(['factory/case/foo/foo.yaml'])
    expect(options.files).to     eq(['factory/case/foo/foo.yaml'])
  end

  it '--case baz has been used from the command line.' do
    ARGV.replace ['--case', 'baz']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_case).to eq(['factory/case/baz/baz.yaml'])
    expect(options.files).to     eq(['factory/case/baz/baz.yaml'])
  end
end

# -------------------------------------------------------------------------- }}}
