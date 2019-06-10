require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related to the 
# ${language-code} keyword.
# ------------------------------------------------------------------------------

shared_examples 'Amber::Substitute.language_code' do |code, language_name|
  it "substitutes \${LANGUAGE-CODE} to #{code}" do
    @options.language = language_name
    expect(Amber::Substitute
      .language_code(@options, '${LANGUAGE-CODE}')).to eql(code)
  end

  it "substitutes \${langauge-code} to #{code}" do
    @options.language = language_name
    expect(Amber::Substitute
      .language_code(@options, '${language-code}')).to eql(code)
  end
end

describe 'YAML Language Code Substitutions' do
  before(:all) do
    @options = Amber::Options.new
  end

  Amber::Language::CODE.each do |code, language_name|
    it_behaves_like 'Amber::Substitute.language_code', code, language_name
  end
end
