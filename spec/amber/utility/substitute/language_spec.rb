# frozen_string_literal: true

require 'amber'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related to the
# ${language} keyword.
# ------------------------------------------------------------------------------
shared_examples 'Amber::Substitute.language' do |code, language_name|
  ARGV.replace ["--language=#{code}"]
  options = Amber::CommandLineOptions.parse(ARGV)

  it "can substitute \${language} to #{language_name}" do
    expect(Amber::Substitute
      .language(options, '${language}')).to eql(language_name)
  end

  it "can substitute \${LANGUAGE} to #{language_name}" do
    expect(Amber::Substitute
      .language(options, '${LANGUAGE}')).to eql(language_name)
  end
end

describe 'YAML Language Substitutions' do
  Amber::Language::CODE.each do |code, language|
    it_behaves_like 'Amber::Substitute.language', code, language
  end
end
