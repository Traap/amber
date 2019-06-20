require 'rspec'
require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related to the 
# ${language} keyword.
# ------------------------------------------------------------------------------
shared_examples 'Amber::Substitute.language' do |language_name|
  it "can substitute \${language} to #{language_name}" do
    @options.language = language_name
    expect(Amber::Substitute.language(@options, '${language}'))
      .to eql(language_name)
  end

  it "can substitute \${LANGUAGE} to #{language_name}" do
    @options.language = language_name
    expect(Amber::Substitute.language(@options, '${LANGUAGE}'))
      .to eql(language_name)
  end
end

describe 'YAML Language Substitutions' do
  before(:all) do
    @options = Amber::Options.new
  end

  Amber::Language::CODE.each do |_code, language|
    it_behaves_like 'Amber::Substitute.language', language
  end
end
