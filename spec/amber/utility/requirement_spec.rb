# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ Requirement substitution.
#
# These Rspecs demonstrate Amber substitution capabilities related to the
# requirement string conversation to an array of requirements.
#
# -------------------------------------------------------------------------- }}}
# {{{ Requirement substitution tests.

describe 'Requirement to Array Transformation' do
  before(:all) do
    @req = '   r1, [r2,r3], and (r4, r5), {r6, r7} and => r8 -> r9  '
  end

  it 'Amber::Requirement.split_on_comma can split a string on a comma to an array' do
    val = Amber::Requirement.split_on_comma(@req)
    expect(val).not_to be nil
    expect(val).to eql(['   r1', ' [r2', 'r3]', ' and (r4', ' r5)', ' {r6', ' r7} and => r8 -> r9  '])
  end

  it 'Amber::Requirement.strip can strip white spaces' do
    val = Amber::Requirement.strip(@req)
    expect(val).to eql('r1, [r2,r3], and (r4, r5), {r6, r7} and => r8 -> r9')
  end

  it 'Amber::Requirement.remove_punctuation can remove punctuation' do
    val = Amber::Requirement.remove_punctuation(@req)
    expect(val).to eql('r1,r2,r3,r4,r5,r6,r7,r8,r9')
  end

  it 'Amber::Requirement.to_array can create a requirement array' do
    val = Amber::Requirement.to_array(@req)
    expect(val).not_to be nil
    expect(val).to eql(%w[r1 r2 r3 r4 r5 r6 r7 r8 r9])
  end
end

# -------------------------------------------------------------------------- }}}
