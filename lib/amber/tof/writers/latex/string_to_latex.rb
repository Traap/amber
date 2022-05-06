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
      use_white_list_names_or_escaped_char(array).to_s
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ use_white_list_names_or_escaped_char

    def self.use_white_list_names_or_escaped_char(array)
      output = ''.dup
      array.each do |item|
        stripped = item.strip
        if Amber::LaTeXWhiteList::NAMES.include? stripped
          output.concat "#{stripped} "
        else
          stripped.each_char { |char| output.concat "#{lookup(char)} " }
        end
      end
      output.strip
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ lookup

    def self.lookup(char)
      found_char = Amber::StringToLaTeXMap::CODE.key(char)
      found_char.nil? ? found_char : char
    end

    # ---------------------------------------------------------------------- }}}
  end
end
