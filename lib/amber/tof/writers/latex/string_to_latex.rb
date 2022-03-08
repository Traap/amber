# rubocop:disable Metrics.ModuleLength
module Amber
  # {{{ Module documentation
  #
  # This module adds a function that can be used to convert any special
  # characters in a given string to the LaTeX friendly forms.  A new string
  # containing the LaTeX form is returned.
  #
  # ------------------------------------------------------------------------ }}}
  module StringToLaTeX
    # rubocop:enable Metrics.ModuleLength
    # {{{ Convert
    #
    # Convert input to a string array to find whitlisted phrases that cannot be
    # converted. Then iterate over each array item replacing any special
    # character with a LaTeX friendly form.

    def self.convert(input)
      output = ''
      array = input.split(/\s/)
      count = array.count
      array.each do |item|
        stripped = item.strip
        if Amber::LaTeXWhiteList::NAMES.include? stripped
          output << stripped
        else
          stripped.each_char { |char| output << lookup(char) }
        end
        count -= 1
        output << ' ' if count >= 1 # Add a space between words.
      end
      output.to_s
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ lookup
    #     template:  when 'a' then '\\a'
    #                when 'Ě' then '\\v{E}'
    # rubocop:disable Metrics.AbcLength
    def self.lookup(char)
      # rubocop:enable Metrics.AbcLength
      case char
      # {{{ Special characters

      when '#'  then '\\#'
      when '$'  then '\\$'
      when '%'  then '\\%'
      when '&'  then '\\&'
      when '^'  then "\\^\ "
      when '_'  then '\\_'
      when '{'  then '\\{'
      when '}'  then '\\}'
      when '~'  then '\\~\\'
      when '|'  then '\\textbar'
      when '\\' then '\\textbackslash'

      # -------------------------------------------------------------------- }}}
      # {{{ Punctuation

      when '«' then '\\guillemotleft'
      when '»' then '\\guillemotright'
      when '‹' then '\\guilsingleft'
      when '›' then '\\guilsinglright'

      # -------------------------------------------------------------------- }}}
      # {{{ Publication marks

      when '©' then '\\textcopyright'
      when '€' then '\\texteuro'
      when '™' then '\\texttrademark'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented A

      when 'á' then "\\'{a}"
      when 'Á' then "\\'{A}"
      when 'à' then '\\`{a}'
      when 'À' then '\\`{A}'
      when 'â' then '\\^{a}'
      when 'Â' then '\\^{A}'
      when 'ä' then '\\\"{a}'
      when 'Ä' then '\\"{A}'
      when 'ã' then '\\~{a}'
      when 'Ã' then '\\~{A}'
      when 'ȧ' then '\\.{a}'
      when 'Ȧ' then '\\.{A}'
      when 'å' then '\\aa'
      when 'Å' then '\\AA'
      when 'ą' then '\\k{a}'
      when 'Ą' then '\\k{A}'
      when 'ă' then '\\u{a}'
      when 'Ă' then '\\u{A}'
      when 'ǎ' then '\\v{a}'
      when 'Ǎ' then '\\v{A}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented C

      when 'ć' then "\\'{c}"
      when 'Ć' then "\\'{C}"
      when 'ç' then '\\c{c}'
      when 'Ç' then '\\c{C}'
      when 'č' then '\\v{c}'
      when 'Č' then '\\v{C}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented D

      when 'ď' then '\\v{d}'
      when 'Ď' then '\\v{D}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented E

      when 'é' then "\\'{e}"
      when 'É' then "\\'{E}"
      when 'è' then '\\`{e}'
      when 'È' then '\\`{E}'
      when 'ê' then '\\^{e}'
      when 'Ê' then '\\^{E}'
      when 'ë' then '\\"{e}'
      when 'Ë' then '\\"{E}'
      when 'ę' then '\\k{e}'
      when 'Ę' then '\\k{E}'
      when 'ĕ' then '\\u{e}'
      when 'ě' then '\\v{e}'
      when 'Ě' then '\\v{E}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented I

      when 'í' then "\\'{i}"
      when 'Í' then "\\'{I}"
      when 'î' then '\\^{i}'
      when 'Î' then '\\^{I}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented L

      when 'ł' then '\\l'
      when 'Ł' then '\\L'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented n

      when 'ń' then "\\'{n}"
      when 'Ń' then "\\'{N}"
      when 'ñ' then '\\~{n}'
      when 'Ñ' then '\\~{N}'
      when 'ň' then '\\v{n}'
      when 'Ň' then '\\v{N}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented O

      when 'ó' then "\\'{o}"
      when 'Ó' then "\\'{O}"
      when 'ô' then '\\^{o}'
      when 'ö' then '\\"{o}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented R

      when 'ř' then '\\v{r}'
      when 'Ř' then '\\v{R}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented S

      when 'ş' then '\\c{s}'
      when 'Ş' then '\\c{S}'
      when 'ś' then "\\'{s}"
      when 'Ś' then "\\'{S}"
      when 'ș' then '\\,{s}'
      when 'Ș' then '\\,{S}'
      when 'š' then '\\v{s}'
      when 'Š' then '\\v{S}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented T

      when 'ţ' then '\\c{t}'
      when 'Ţ' then '\\c{T}'
      when 'ț' then '\\,{t}'
      when 'Ț' then '\\,{T}'
      when 'ť' then '\\v{t}'
      when 'Ť' then '\\v{T}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented U

      when 'ů' then '\\r{u}'
      when 'Ů' then '\\r{U}'
      when 'ú' then "\\'{u}"
      when 'Ú' then "\\'{U}"
      when 'ü' then '\\"{u}'
      when 'Ü' then '\\"{U}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented Y

      when 'ý' then "\\'{y}"
      when 'Ý' then "\\'{Y}"

      # -------------------------------------------------------------------- }}}
      # {{{ Accented Z

      when 'ž' then '\\v{z}'
      when 'Ž' then '\\v{Z}'
      when 'ź' then "\\'{z}"
      when 'Ź' then "\\'{Z}"
      when 'ż' then '\\.{z}'
      when 'Ż' then '\\.{Z}'

      # -------------------------------------------------------------------- }}}
      # {{{ Accented double char.

      when 'æ' then '\\ae'
      when 'Æ' then '\\AE'
      when 'ð' then '\\dh'
      when 'Ð' then '\\DH'
      when 'đ' then '\\dj'
      when 'ŋ' then '\\ng'
      when 'Ŋ' then '\\NG'
      when 'œ' then '\\oe'
      when 'Œ' then '\\OE'
      when 'ø' then '\\o'
      when 'Ø' then '\\O'
      when 'ß' then '\\ss'
      when 'ẞ' then '\\SS'

      # -------------------------------------------------------------------- }}}
      # {{{ Do nothing

      else char

        # ------------------------------------------------------------------ }}}
      end
    end
    # ---------------------------------------------------------------------- }}}
  end
end
