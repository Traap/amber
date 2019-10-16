module Amber 
  # This module adds a function that can be used to convert any special 
  # characters in a given string to the LaTeX friendly forms.  a new string
  # containing the LaTeX form is returned.
  module StringToLatex

    def self.convert_to_latex(input_string)
      latex_string = ''
      input_string.each_char { |char| latex_string << lookup_latex(char) }
      latex_string
    end

    def self.lookup_latex(char)
      # template:  when 'a' then  '\\a'
      case char
      when '&' then '\\&\ '
      when '#' then '\\#\ '
      when '%' then '\\%\ '
      when '$' then '\\$\ '
      when '_' then '\\_\ '
      when '~' then '\\~\ '
      when '^' then '\\^\ '
      else char
      end
    end
  end
end
