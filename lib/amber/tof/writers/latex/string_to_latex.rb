module Amber 
  # This module adds a function that can be used to convert any special
  # characters in a given string to the LaTeX friendly forms.  A new string
  # containing the LaTeX form is returned.
  module StringToLatex

    # Iterate over each character in input_string replacing any special 
    # character with a LaTeX friendly form.
    def self.convert_to_latex(input_string)
      latex_string = ''
      input_string.each_char { |char| latex_string << lookup_latex(char) }
      latex_string
    end

    def self.lookup_latex(char)
      # template:  when 'a' then  '\\a\ '
      #            when 'Ě' then '\\v{E} '
      # Note: A white space is added to prevent substitution side effects. 
      case char
      when '#' then '\\#\ '
      when '$' then '\\$\ '
      when '%' then '\\%\ '
      when '&' then '\\&\ '
      when '^' then '\\^\ '
      when '_' then '\\_\ '
      when '{' then '\\{\ '
      when '}' then '\\}\ '
      when '~' then '\\~\ '
      when '\\' then '\\textbackslash\ '
      when '©' then '\\textcopyright\ '
      when '«' then '\\guillemotleft\ '
      when '»' then '\\guillemotright\ '
      when '‹' then '\\guilsingleft\ '
      when '›' then '\\guilsingright\ '
      when '|' then '\\textbar\ '
      when '€' then '\\texteuro\ '
      when '>' then '\\textgreater\ '
      when '<' then '\\textless\ '
      when '™' then '\\texttrademark\ '
      when 'Ě' then '\\v{E} '
      when 'Ď' then '\\v{D} '
      when 'Ó' then "\\'{O} "
      when 'Ȧ' then '\\.{A} '
      when 'Å' then '\\AA{} '
      when 'Ä' then '\\"{A} '
      when 'À' then '\\`{A} '
      when 'Á' then "\\'{A} "
      when 'Â' then '\\^{A} '
      when 'Ǎ' then '\\v{A} '
      when 'Ă' then '\\u{A} '
      when 'Ã' then '\\~{A} '
      when 'Ą' then '\\k{A} '
      when 'ȧ' then '\\.{a} '
      when 'ä' then '\\\"{a} '
      when 'å' then '\\aa{} '
      when 'à' then '\\`{a} '
      when 'á' then "\\'{a} "
      when 'â' then '\\^{a} '
      when 'ǎ' then '\\v{a} '
      when 'ă' then '\\u{a} '
      when 'ã' then '\\~{a} '
      when 'ą' then '\\k{a} '
      when 'ç' then '\\c{c} '
      when 'ć' then "\\'{c} "
      when 'č' then '\\v{c} '
      when 'Ç' then '\\c{C} '
      when 'Ć' then "\\'{C} "
      when 'Č' then '\\v{C} '
      when 'ď' then '\\v{d} '
      when 'ê' then '\\^{e} '
      when 'é' then "\\'{e} "
      when 'ë' then '\\\"{e} '
      when 'è' then '\\`{e} '
      when 'ě' then '\\v{e} '
      when 'ĕ' then '\\u{e} '
      when 'ę' then '\\k{e} '
      when 'Ê' then '\\^{E} '
      when 'Ę' then '\\k{E} '
      when 'É' then "\\'{E} "
      when 'Ë' then '\\\"{E} '
      when 'È' then '\\`{E} '
      when 'í' then "\\'{i} "
      when 'Í' then "\\'{I} "
      when 'î' then '\\^{i} '
      when 'Î' then '\\^{I} '
      when 'ł' then '\\l{} '
      when 'Ł' then '\\L{} '
      when 'ñ' then '\\~{n} '
      when 'Ñ' then '\\~{N} '
      when 'ń' then "\\'{n} "
      when 'Ń' then "\\'{N} "
      when 'ň' then '\\v{n} '
      when 'Ň' then '\\v{N} '
      when 'ô' then '\\^{o} '
      when 'ö' then '\\\"{o} '
      when 'ó' then "\\'{o} "
      when 'ř' then '\\v{r} '
      when 'Ř' then '\\v{R} '
      when 'ş' then '\\c{s} '
      when 'š' then '\\v{s} '
      when 'ș' then '\\,{s} '
      when 'ś' then "\\'{s} "
      when 'Ş' then '\\c{S} '
      when 'Ś' then "\\'{S} "
      when 'Š' then '\\v{S} '
      when 'Ș' then '\\,{S} '
      when 'ț' then '\\,{t} '
      when 'ť' then '\\v{t} '
      when 'ţ' then '\\c{t} '
      when 'Ť' then '\\v{T} '
      when 'Ț' then '\\,{T} '
      when 'Ţ' then '\\c{T} '
      when 'ú' then "\\'{u} "
      when 'ü' then '\\"{u} '
      when 'ů' then '\\r{u} '
      when 'Ú' then "\\'{U} "
      when 'Ü' then '\\"{U} '
      when 'Ů' then '\\r{U} '
      when 'ý' then "\\'{y} "
      when 'Ý' then "\\'{Y} "
      when 'ž' then '\\v{z} '
      when 'ź' then "\\'{z} "
      when 'ż' then '\\.{z} '
      when 'Ź' then "\\'{Z} "
      when 'Ż' then '\\.{Z} '
      when 'Ž' then '\\v{Z} '
      when 'Æ' then '\\AE{} '
      when 'æ' then '\\ae{} '
      when 'Œ' then '\\OE{} '
      when 'œ' then '\\oe{} '
      when 'Ø' then '\\O{} '
      when 'ø' then '\\o{} '
      when 'Ð' then '\\DH{} '
      when 'ð' then '\\dh{} '
      when 'đ' then '\\dj{} '
      when 'Ŋ' then '\\NG{} '
      when 'ŋ' then '\\ng{} '
      when 'ß' then '\\ss{} '
      when 'ẞ' then '\\SS{} '

      else char
      end
    end
  end
end
