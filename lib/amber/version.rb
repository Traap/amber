# frozen_string_literal: true

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
#     The patch number is incremented each time the amber gem is built and
#     installed.  This number never resets.  Not all patches are released to the
#     public domain.
#
# ------------------------------------------------------------------------------
module Amber
  VERSION = '1.5.1.307'
end
# ------------------------------------------------------------------------------
