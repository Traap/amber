require 'fileutils'
require 'amber/options'
require 'amber/language'

module Amber
  module Substitute
    def self.strings(filename, options, text)
      unless text.nil?
        Substitute.home(
          Substitute.file(filename,
                          Substitute.browser(options,
                                             Substitute.language(options,
                                                                 Substitute.language_code(options, text))))
        )
      end
    end

    def self.file(filename, text)
      text.gsub(/(\${file}|\${FILE})/, File.basename(filename, '.*'))
    end

    def self.browser(options, text)
      text.gsub(/(\${browser}|\${BROWSER})/, options.browser.to_s)
    end

    def self.language(options, text)
      text.gsub(/(\${language}|\${LANGUAGE})/, options.language.to_s)
    end

    def self.language_code(options, text)
      text.gsub(/(\${language-code}|\${LANGUAGE-CODE})/,
                Amber::Language::Code.key(options.language).to_s)
    end

    def self.home(text)
      text.gsub(/(\${home}|\${HOME})/, '~')
    end

    def self.expand_path(text)
      unless text.nil?
        expanded_text = ''
        words = text.split(' ')
        words.each do |w|
          w = Substitute.home(w)
          if w.start_with?('~')
            expanded_text.concat File.expand_path(w)
          else
            expanded_text.concat w
         end
          expanded_text.concat ' '
        end
        expanded_text
      end
    end
  end
end
