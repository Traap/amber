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
      array = input.split(/\s/)
      use_whitelist_name_or_escaped_value_or_self(array, ''.dup)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ use_whitelist_name_or_escaped_value_or_self
    #
    # array - An array string representing LaTeX characters that maybe white
    #         listed or require escaped with LaTeX punctuation.
    # output - is a duped string that is modified and returned.

    def self.use_whitelist_name_or_escaped_value_or_self(array, output)
      array.each_index do |index|
        name = array[index].strip
        if Amber::LaTeXWhiteList::NAMES.include? name
          output << name
        else
          name.each_char { |char| output << escaped_value_or_self(char) }
        end
        output << ' ' if index < (array.count - 1)
      end
      output.to_s
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ escaped_value_or_self

    def self.escaped_value_or_self(char)
      found_char = Amber::StringToLaTeXMap::CODE[char]
      found_char.nil? ? char : found_char
    end

    # ---------------------------------------------------------------------- }}}
  end
end
