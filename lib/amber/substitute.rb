require 'fileutils'
require 'amber/options'
require 'amber/language'

module Amber
  module Substitute

    def Substitute.strings(options, text)
      Substitute.file(options,
        Substitute.browser(options,
          Substitute.language(options,
            Substitute.language_code(options, text))))
    end

    def Substitute.file(options, text)
      text.gsub(/\${file}/, "#{File.basename(options.filename.first, ".*")}")
    end

    def Substitute.browser(options, text)
      text.gsub(/\${browser}/, "#{options.browser}")
    end

    def Substitute.language(options, text)
      text.gsub(/\${language}/, "#{options.language}")
    end

    def Substitute.language_code(options, text)
      text.gsub(/\${language-code}/, 
                "#{Amber::Language::Map.key(options.language)}")
    end

  end # Substitute
end # Amber
