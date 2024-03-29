# frozen_string_literal: true

# {{{ LaTeX characters that need special handling
#
# -------------------------------------------------------------------------- }}}
module Amber
  module StringToLaTeXMap # rubocop:disable Metrics.BlockLength
    # {{{ List each support mapping.
    #
    #     template: 'a' => '\\a'
    #               'Ě' => '\\v{E}'

    CODE = {
      # {{{ Special characters

      '#' => '\\#',
      '$' => '\\$',
      '%' => '\\%',
      '^' => "\\^\ ",
      '_' => '\\_',
      '~' => '\\~\\',

      # -------------------------------------------------------------------- }}}
      # {{{ Punctuation

      '«' => '\\guillemotleft',
      '»' => '\\guillemotright',
      '‹' => '\\guilsingleft',
      '›' => '\\guilsinglright',

      # -------------------------------------------------------------------- }}}
      # {{{ Publication marks

      '©' => '\\textcopyright',
      '€' => '\\texteuro',
      '™' => '\\texttrademark',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented A

      'á' => "\\'{a}",
      'Á' => "\\'{A}",
      'à' => '\\`{a}',
      'À' => '\\`{A}',
      'â' => '\\^{a}',
      'Â' => '\\^{A}',
      'ä' => '\\\"{a}',
      'Ä' => '\\"{A}',
      'ã' => '\\~{a}',
      'Ã' => '\\~{A}',
      'ȧ' => '\\.{a}',
      'Ȧ' => '\\.{A}',
      'å' => '\\aa',
      'Å' => '\\AA',
      'ą' => '\\k{a}',
      'Ą' => '\\k{A}',
      'ă' => '\\u{a}',
      'Ă' => '\\u{A}',
      'ǎ' => '\\v{a}',
      'Ǎ' => '\\v{A}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented C

      'ć' => "\\'{c}",
      'Ć' => "\\'{C}",
      'ç' => '\\c{c}',
      'Ç' => '\\c{C}',
      'č' => '\\v{c}',
      'Č' => '\\v{C}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented D

      'ď' => '\\v{d}',
      'Ď' => '\\v{D}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented E

      'é' => "\\'{e}",
      'É' => "\\'{E}",
      'è' => '\\`{e}',
      'È' => '\\`{E}',
      'ê' => '\\^{e}',
      'Ê' => '\\^{E}',
      'ë' => '\\"{e}',
      'Ë' => '\\"{E}',
      'ę' => '\\k{e}',
      'Ę' => '\\k{E}',
      'ĕ' => '\\u{e}',
      'ě' => '\\v{e}',
      'Ě' => '\\v{E}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented I

      'í' => "\\'{i}",
      'Í' => "\\'{I}",
      'î' => '\\^{i}',
      'Î' => '\\^{I}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented L

      'ł' => '\\l',
      'Ł' => '\\L',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented n

      'ń' => "\\'{n}",
      'Ń' => "\\'{N}",
      'ñ' => '\\~{n}',
      'Ñ' => '\\~{N}',
      'ň' => '\\v{n}',
      'Ň' => '\\v{N}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented O

      'ó' => "\\'{o}",
      'Ó' => "\\'{O}",
      'ô' => '\\^{o}',
      'ö' => '\\"{o}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented R

      'ř' => '\\v{r}',
      'Ř' => '\\v{R}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented S

      'ş' => '\\c{s}',
      'Ş' => '\\c{S}',
      'ś' => "\\'{s}",
      'Ś' => "\\'{S}",
      'ș' => '\\,{s}',
      'Ș' => '\\,{S}',
      'š' => '\\v{s}',
      'Š' => '\\v{S}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented T

      'ţ' => '\\c{t}',
      'Ţ' => '\\c{T}',
      'ț' => '\\,{t}',
      'Ț' => '\\,{T}',
      'ť' => '\\v{t}',
      'Ť' => '\\v{T}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented U

      'ů' => '\\r{u}',
      'Ů' => '\\r{U}',
      'ú' => "\\'{u}",
      'Ú' => "\\'{U}",
      'ü' => '\\"{u}',
      'Ü' => '\\"{U}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented Y

      'ý' => "\\'{y}",
      'Ý' => "\\'{Y}",

      # -------------------------------------------------------------------- }}}
      # {{{ Accented Z

      'ž' => '\\v{z}',
      'Ž' => '\\v{Z}',
      'ź' => "\\'{z}",
      'Ź' => "\\'{Z}",
      'ż' => '\\.{z}',
      'Ż' => '\\.{Z}',

      # -------------------------------------------------------------------- }}}
      # {{{ Accented double char.

      'æ' => '\\ae',
      'Æ' => '\\AE',
      'ð' => '\\dh',
      'Ð' => '\\DH',
      'đ' => '\\dj',
      'ŋ' => '\\ng',
      'Ŋ' => '\\NG',
      'œ' => '\\oe',
      'Œ' => '\\OE',
      'ø' => '\\o',
      'Ø' => '\\O',
      'ß' => '\\ss',
      'ẞ' => '\\SS'

      # -------------------------------------------------------------------- }}}
    }.freeze

    # ---------------------------------------------------------------------- }}}
    # {{{ Freeze CODE.keys

    NAMES = CODE.keys.freeze

    # ---------------------------------------------------------------------- }}}
  end
end
