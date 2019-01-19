# Amber Requirement

module Amber
  module Requirement 
    def self.to_array(text)
      unless text.nil?
        Requirement.split_on_comma(
         Requirement.strip(
          Requirement.remove_punctuation(text)))
      end
    end

    def self.split_on_comma(text)
      text.split(',')
    end

    def self.strip(text)
      text.strip
    end

    def self.remove_punctuation(text)
      text
        .gsub(/(\{|\(|\[)/, ',')
        .gsub(/(\}|\)|\])/, '')
        .gsub(/(;|\->|=>|, and | and )/, ',')
        .gsub(/ /, '')
        .gsub(/,,/, ',')
    end

  end
end
