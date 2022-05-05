# frozen_string_literal: true

# {{{ Documentation

#   Amber Substitutes the following strings when process a YAML files.
#
#   The name of the YAML file.
#     ${file}
#     ${FILE}
#
#   The name of the browers.
#     ${browser}
#     ${BROWSER}
#
#   The name of the language.
#     ${language}
#     ${LANGUAGE}
#
#   The language code.
#     ${language-code}
#     ${language-code}
#
#   The home directory
#     ~
#     ${home}
#     ${HOME}
# -------------------------------------------------------------------------- }}}
# {{{ Required files.

require 'fileutils'
require 'amber/cli/options'
require 'amber/cli/language'

# -------------------------------------------------------------------------- }}}
module Amber
  module Substitute
    # {{{ strings

    def self.strings(filename, options, text)
      return if text.nil?

      # rubocop:disable Layout.ArgumenhtAlignment
      Substitute.home(
        Substitute.file(filename,
          Substitute.browser(options,
            Substitute.language(options,
              Substitute.language_code(options, text))))
      )
      # rubocop:enable Layout.ArgumenhtAlignment
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ files

    def self.file(filename, text)
      name = case filename
             when String
               filename
             else
               filename.first.to_s
             end
      text.gsub(/(\${file}|\${FILE})/, File.basename(name, '.*'))
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ browser

    def self.browser(options, text)
      text.gsub(/(\${browser}|\${BROWSER})/, options.browser.to_s)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ language

    def self.language(options, text)
      text.gsub(/(\${language}|\${LANGUAGE})/, options.language.to_s)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ language_code

    def self.language_code(options, text)
      text.gsub(/(\${language-code}|\${LANGUAGE-CODE})/,
                Amber::Language::CODE.key(options.language).to_s)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ home

    def self.home(text)
      text.gsub(/(\${home}|\${HOME})/, '~')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ expand_path

    def self.expand_path(text)
      return if text.nil?

      expanded_text = ''
      words = text.split(' ')
      words.each do |word|
        expanded_text = expand_text(word)
      end
      expanded_text
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ expand_text

    def expand_text(text)
      word = Substitute.home(text)
      expanded_text.concat = if sword.start_with?('~')
                               "#{File.expand_path(word)} "
                             else
                               "#{word} "
                             end
    end

    # ---------------------------------------------------------------------- }}}
  end
end
