# frozen_string_literal: true

# {{{ Gems need throughout Amber.

require 'open3'
require 'optparse'
require 'ostruct'
require 'pp'
require 'rbconfig'
require 'rspec'
require 'yaml'

# -------------------------------------------------------------------------- }}}
# {{{ Making it easy to set up Amber includes and test Amber with RSpec.

require 'require_all'
require_rel ''

# -------------------------------------------------------------------------- }}}
# {{{ RSpec test coverage.

require 'simplecov'

# -------------------------------------------------------------------------- }}}
