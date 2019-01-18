require 'rspec'
require 'amber'

# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related to the 
# requirement string conversation to an array of requirements.
# ------------------------------------------------------------------------------
describe 'Requirement to Array Transformation' do

  before(:all) do
    @req = '   r1, [r2,r3], (r4, r5), {r6, r7} => r8 -> r9  '
  end

  describe 'Amber::Requirement.split_on_comma' do
    it 'can split a string on a comma to an array' do
      val = Amber::Requirement.split_on_comma(@req)
      expect(val).not_to be nil
      expect(val).to eql(['   r1', ' [r2', 'r3]', ' (r4', ' r5)', ' {r6', ' r7} => r8 -> r9  '])
    end
  end

  describe 'Amber::Requirement.strip' do
    it 'can strip white spaces' do
      val = Amber::Requirement.strip(@req)
      expect(val).to eql('r1, [r2,r3], (r4, r5), {r6, r7} => r8 -> r9')
    end
  end

  describe 'Amber::Requirement.remove_punctuation ' do
    it 'can remove punctuation' do
      val = Amber::Requirement.remove_punctuation(@req)
      expect(val).to eql('r1,r2,r3,r4,r5,r6,r7,r8,r9')
    end
  end

  describe 'Amber::Requirement.to_array' do
    it 'can create a requirement array' do
      val = Amber::Requirement.to_array(@req)
      expect(val).not_to be nil
      expect(val).to eql(['r1', 'r2', 'r3', 'r4', 'r5', 'r6', 'r7', 'r8', 'r9'])
    end
  
  end
end
