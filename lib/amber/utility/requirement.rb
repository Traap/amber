# frozen_string_literal: true

module Amber
  module Requirement
    # {{{ to_array

    def self.to_array(text)
      return if text.nil?

      Requirement.split_on_comma(
        Requirement.strip(
          Requirement.remove_punctuation(text)
        )
      )
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ split_on_comma

    def self.split_on_comma(text)
      text.split(',')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ strip

    def self.strip(text)
      text.strip
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ remove_punctuation

    def self.remove_punctuation(text)
      text
        .gsub(/(\{|\(|\[)/, ',')
        .gsub(/(\}|\)|\])/, '')
        .gsub(/(;|->|=>|, and | and )/, ',')
        .gsub(/ /, '')
        .gsub(/,,/, ',')
    end

    # ---------------------------------------------------------------------- }}}
  end
end
