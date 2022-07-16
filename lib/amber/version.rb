# frozen_string_literal: true

# {{{ Version number.
#
# See https://semver.org/
#
# Major, Minor, Patch, Build
#   Major
#     Interface and backward compatibility is not guaranteed.
#
#   Minor
#     Interface may change and backward compatibility is guaranteed.
#
#   Patch
#     A backward compatible bug fix or refactoring to improve maintenance.
#
#   Build
#     The build number is incremented each time the amber gem is built and
#     installed.  This number never resets.  Not all builds are released to the
#     public domain.

module Amber
  VERSION = '1.6.4.412'
end

# -------------------------------------------------------------------------- }}}
