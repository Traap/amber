require 'rspec'
# ------------------------------------------------------------------------------
# These Rspecs demonstrate Amber substitution capabilities related to the 
# string function. 
# ------------------------------------------------------------------------------
describe 'YAML Strings Substitutions' do

  describe 'Amber::Substitute.strings' do

    ARGV.replace [
      "--browser",  "Opera", 
      "--file",     "foo/bar/baz/baz.yaml", 
      "--language", "sv"
    ]

    options = Amber::CommandLineOptions.parse(ARGV)

    it 'can substitute ${BROWSER} to Opera' do
      expect(Amber::Substitute
        .strings(
          options.filename, 
          options, 
          '${BROWSER}'))
        .to eql('Opera')
    end

    it 'can substitute ${browser} ${file} ${language} and ${language-code} to Opera baz Svenska and sv' do
      expect(Amber::Substitute
        .strings(
          options.filename, 
          options, 
          '${browser} ${file} ${language} and ${language-code}'))
        .to eql('Opera baz Svenska and sv')
    end

    it 'can substitute ${language-code}${file}${language}${browser} to svbazSvenskaOpera' do
      expect(Amber::Substitute
        .strings(
          options.filename, 
          options, 
          '${language-code}${file}${language}${browser}'))
        .to eql('svbazSvenskaOpera')
    end

  end
end
