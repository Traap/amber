require 'fileutils'
require 'amber/options'
require 'amber/language'

module Amber
  module Substitute

    def Substitute.strings(options, text)
      if !text.nil? then
        Substitute.home(
          Substitute.file(options,
            Substitute.browser(options,
              Substitute.language(options,
                Substitute.language_code(options, text)))))
      else 
        nil
      end
    end

    def Substitute.file(options, text)
      text.gsub(/\${file}/, "#{File.basename(options.filename.first, ".*")}")
      text.gsub(/\${FILE}/, "#{File.basename(options.filename.first, ".*")}")
    end

    def Substitute.browser(options, text)
      text.gsub(/\${browser}/, "#{options.browser}")
      text.gsub(/\${BROWSER}/, "#{options.browser}")
    end

    def Substitute.language(options, text)
      text.gsub(/\${language}/, "#{options.language}")
      text.gsub(/\${LANGUAGE}/, "#{options.language}")
    end

    def Substitute.language_code(options, text)
      text.gsub(/\${language-code}/, 
                "#{Amber::Language::Code.key(options.language)}")
      text.gsub(/\${LANGUAGE-CODE}/, 
                "#{Amber::Language::Code.key(options.language)}")
    end

    def Substitute.home(text)
      text.gsub(/\${home}/, "~")
      text.gsub(/\${HOME}/, "~")
    end

    def Substitute.expand_path(text)
      if !text.nil? then
        expanded_text = "" 
        words = text.split(' ')
        words.each do |w|
          w = Substitute.home(w)
          if w.start_with?("~") then
            expanded_text.concat File.expand_path(w)
          else
            expanded_text.concat w
         end 
         expanded_text.concat " "
        end
        expanded_text
      else
        nil
      end
    end

  end # Substitute
end # Amber
