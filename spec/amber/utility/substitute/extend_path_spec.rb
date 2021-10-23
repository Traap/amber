# frozen_string_literal: true

require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related the ~ marker.
# ------------------------------------------------------------------------------
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
