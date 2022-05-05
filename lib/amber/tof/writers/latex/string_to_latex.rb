# frozen_string_literal: true

require 'amber/tof/writers/latex/string_to_latex_map'

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
      output = ''.dup
      array = input.split(/\s/)
      strip_white_listed_names(array, array.count, output)
      output.to_s
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ strip_white_listed_names

    def self.strip_white_listed_names(array, count, output)
      array.each do |item|
        stripped = item.strip
        if Amber::LaTeXWhiteList::NAMES.include? stripped
          output.concat stripped
        else
          stripped.each_char { |char| output.concat lookup(char) }
        end
        count -= 1
        output.concat ' ' if count >= 1 # Add a space between words.
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ lookup

    def self.lookup(char)
      Amber::StringToLaTeXMap::CODE.key(char)
    end

    # ---------------------------------------------------------------------- }}}
  end
end
