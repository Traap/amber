# frozen_string_literal: true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ All keyword substitutions.
#
# These Rspecs demonstrate Amber substitution capabilities related to the
# ${browser} keyword.
#
# -------------------------------------------------------------------------- }}}
# {{{ All keyword substitution tests.

describe 'YAML Strings Substitutions' do
  describe 'Amber::Substitute.strings' do
    ARGV.replace [
      '--browser',  'Opera',
      '--file',     'foo/bar/baz/baz.yaml',
      '--language', 'sv'
    ]

    options = Amber::CommandLineOptions.parse(ARGV)

    it 'can substitute ${BROWSER} to Opera' do
      expect(Amber::Substitute
        .strings(options.filename, options, '${BROWSER}'))
        .to eql('Opera')
    end

    it 'can substitute ${browser} ${file} ${language} and ${language-code} to Opera baz Swedish and sv' do
      expect(Amber::Substitute
        .strings(options.filename, options, '${browser} ${file} ${language} and ${language-code}'))
        .to eql('Opera baz Swedish and sv')
    end

    it 'can substitute ${language-code}${file}${language}${browser} to svbazSwedishOpera' do
      expect(Amber::Substitute
        .strings(options.filename, options, '${language-code}${file}${language}${browser}'))
        .to eql('svbazSwedishOpera')
    end
  end
end

# -------------------------------------------------------------------------- }}}
