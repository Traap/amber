# frozen_string_literal: true

module Amber
  # {{{ Module documentation
  #
  # This module adds a function that can be used to convert any special
  # characters in a given string to the LaTeX friendly forms.  A new string
  # containing the LaTeX form is returned.
  #
  # ------------------------------------------------------------------------ }}}
  module StringToLaTeX
    # {{{ Convert
    #
    # Convert input to a string array to find whitlisted phrases that cannot be
    # converted. Then iterate over each array item replacing any special
    # character with a LaTeX friendly form.

    def self.convert(input)
      output = ''
      array = input.split(/\s/)
      output.append strip_white_listed_characters(array, array.count)
      output.to_s
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ strip_white_listed_characters

    def strip_white_listed_characters(array, count)
      array.each do |item|
        stripped = item.strip
        if Amber::LaTeXWhiteList::NAMES.include? stripped
          output.append stripped
        else
          stripped.each_char { |char| output.append lookup(char) }
        end
        count -= 1
        output.append ' ' if count >= 1 # Add a space between words.
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ lookup

    def self.lookup(char)
      Amber::StringToLaTexMap::CODE.key(char)
    end

    # ---------------------------------------------------------------------- }}}
  end
end
