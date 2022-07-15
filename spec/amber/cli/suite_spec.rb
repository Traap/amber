# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Suite options.
#
# [-s | --suite
#
# [foo | bar | baz]
#
# Fully-qualified extention at runtime:
#   factory/suite/foo/foo.yaml
#   factory/suite/bar/bar.yaml
#   factory/suite/baz/baz.yaml
#
# -------------------------------------------------------------------------- }}}
# {{{ Suite tests short commands.

describe 'Amber CLO Suite Short' do
  it '-s has not been used.' do
    options = Amber::Options.new
    expect(options.test_suite).to eq(nil)
    expect(options.files).to      eq(nil)
  end

  it '-sbar has been used from the command line.' do
    ARGV.replace ['-sbar']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_suite).to eq(['factory/suite/bar/bar.yaml'])
    expect(options.files).to      eq(['factory/suite/bar/bar.yaml'])
  end

  it '-s foobar has been used from the command line.' do
    ARGV.replace ['-s', 'foobar']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_suite).to eq(['factory/suite/foobar/foobar.yaml'])
    expect(options.files).to      eq(['factory/suite/foobar/foobar.yaml'])
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Suite tests long commands.

describe 'Amber CLO Suite Long' do
  it '--suite has not been used.' do
    options = Amber::Options.new
    expect(options.test_suite).to eq(nil)
    expect(options.files).to      eq(nil)
  end

  it '--suite=foo has been used from the command line.' do
    ARGV.replace ['--suite=foo']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_suite).to eq(['factory/suite/foo/foo.yaml'])
    expect(options.files).to      eq(['factory/suite/foo/foo.yaml'])
  end

  it '--suite baz has been used from the command line.' do
    ARGV.replace ['--suite', 'baz']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_suite).to eq(['factory/suite/baz/baz.yaml'])
    expect(options.files).to      eq(['factory/suite/baz/baz.yaml'])
  end
end

# -------------------------------------------------------------------------- }}}
