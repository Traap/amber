# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Path substitutions.
#
# These Rspecs demonstrate Amber substitution capabilities related the ~ marker.
#
# -------------------------------------------------------------------------- }}}
# {{{ Path substitutions tests.

describe 'YAML Extend Path Substitutions' do
  describe 'Amber::Substitute.expected_path' do
    it "can expand ~ to #{Dir.home}" do
      expect(Amber::Substitute.expand_path('~').strip).to eql(Dir.home)
    end

    it "can expand ~ and ~ to #{Dir.home} and #{Dir.home}" do
      expect(Amber::Substitute
        .expand_path('~ and ~').strip).to eql("#{Dir.home} and #{Dir.home}")
    end
  end
end

# -------------------------------------------------------------------------- }}}
