# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Plan options.
#
# [-p | --plan
#
# [foo | bar | baz]
#
# Fully-qualified extention at runtime:
#   factory/plan/foo/foo.yaml
#   factory/plan/bar/bar.yaml
#   factory/plan/baz/baz.yaml
#
# -------------------------------------------------------------------------- }}}
# {{{ Plan tests short commands.

describe 'Amber CLO Plan Short' do
  it '-p has not been used.' do
    options = Amber::Options.new
    expect(options.test_plan).to eq(nil)
    expect(options.files).to     eq(nil)
  end

  it '-pbar has been used from the command line.' do
    ARGV.replace ['-pbar']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_plan).to eq(['factory/plan/bar/bar.yaml'])
    expect(options.files).to     eq(['factory/plan/bar/bar.yaml'])
  end

  it '-p foobar has been used from the command line.' do
    ARGV.replace ['-p', 'foobar']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_plan).to eq(['factory/plan/foobar/foobar.yaml'])
    expect(options.files).to     eq(['factory/plan/foobar/foobar.yaml'])
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Plan tests long commands.

describe 'Amber CLO Plan Long' do
  it '--plan has not been used.' do
    options = Amber::Options.new
    expect(options.test_plan).to eq(nil)
    expect(options.files).to     eq(nil)
  end

  it '--plan=foo has been used from the command line.' do
    ARGV.replace ['--plan=foo']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_plan).to eq(['factory/plan/foo/foo.yaml'])
    expect(options.files).to     eq(['factory/plan/foo/foo.yaml'])
  end

  it '--plan baz has been used from the command line.' do
    ARGV.replace ['--plan', 'baz']
    options = Amber::CommandLineOptions.parse(ARGV)
    expect(options.test_plan).to eq(['factory/plan/baz/baz.yaml'])
    expect(options.files).to     eq(['factory/plan/baz/baz.yaml'])
  end
end

# -------------------------------------------------------------------------- }}}
