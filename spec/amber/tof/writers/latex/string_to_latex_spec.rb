# frozen_string_literal:true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ check_conversion.

def check_conversion(input_string, output_string)
  out = Amber::StringToLaTeX.convert(input_string)
  expect(out).to eq(output_string)
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert Shared examples

shared_examples 'convert' do |description, input, output|
  context "#{description}: from #{input} to #{output}" do
    subject { Amber::StringToLaTeX.convert(input) }
    it { should eq(output) }
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: escapable characters.

describe 'String to LaTeX' do
  describe 'converts escapable characters' do
    it_should_behave_like 'convert', 'ampersand', '&', '&'
    it_should_behave_like 'convert', 'caret', '^', "\\^\ "
    it_should_behave_like 'convert', 'dollar', '$', '\\$'
    it_should_behave_like 'convert', 'hash', '#', '\\#'
    it_should_behave_like 'convert', 'left brace', '{', '{'
    it_should_behave_like 'convert', 'percent', '%', '\\%'
    it_should_behave_like 'convert', 'right brace', '}', '}'
    it_should_behave_like 'convert', 'tilde', '~', '\\~\\'
    it_should_behave_like 'convert', 'underscore', '_', '\\_'
    it_should_behave_like 'convert', 'backslash', '\\', '\\'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Copyright.

describe 'String to LaTeX' do
  describe 'converts copyright characters' do
    it_should_behave_like 'convert', 'textbar', '|', '|'
    it_should_behave_like 'convert', 'copyright', '©', '\\textcopyright'
    it_should_behave_like 'convert', 'euro', '€', '\\texteuro'
    it_should_behave_like 'convert', 'texttrademark', '™', '\\texttrademark'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Guillemot.

describe 'String to LaTeX' do
  describe 'converts guillemot characters' do
    it_should_behave_like 'convert', 'left guillemot', '«', '\\guillemotleft'
    it_should_behave_like 'convert', 'right guillemot', '»', '\\guillemotright'
    it_should_behave_like 'convert', 'left guillsing', '‹', '\\guilsingleft'
    it_should_behave_like 'convert', 'right guillsing', '›', '\\guilsinglright'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented A.

describe 'String to LaTeX' do
  describe 'converts accented A letters' do
    it_should_behave_like 'convert', 'hat A', 'Â', '\\^{A}'
    it_should_behave_like 'convert', 'check A', 'Ǎ', '\\v{A}'
    it_should_behave_like 'convert', 'ring A', 'Å', '\\AA'
    it_should_behave_like 'convert', 'umlaut A', 'Ä', '\\"{A}'
    it_should_behave_like 'convert', 'grave A', 'À', '\\`{A}'
    it_should_behave_like 'convert', 'acute A', 'Á', "\\'{A}"
    it_should_behave_like 'convert', 'tilde A', 'Ã', '\\~{A}'
    it_should_behave_like 'convert', 'ogonek A', 'Ą', '\\k{A}'
    it_should_behave_like 'convert', 'breve A', 'Ă', '\\u{A}'
    it_should_behave_like 'convert', 'dot A', 'Ȧ', '\\.{A}'
    it_should_behave_like 'convert', 'hat a', 'â', '\\^{a}'
    it_should_behave_like 'convert', 'check a', 'ǎ', '\\v{a}'
    it_should_behave_like 'convert', 'breve a', 'ă', '\\u{a}'
    it_should_behave_like 'convert', 'ring a', 'å', '\\aa'
    it_should_behave_like 'convert', 'umlaut a', 'ä', '\\\"{a}'
    it_should_behave_like 'convert', 'grave a', 'à', '\\`{a}'
    it_should_behave_like 'convert', 'acute a', 'á', "\\'{a}"
    it_should_behave_like 'convert', 'tilde a', 'ã', '\\~{a}'
    it_should_behave_like 'convert', 'ogonek a', 'ą', '\\k{a}'
    it_should_behave_like 'convert', 'dot A', 'ȧ', '\\.{a}'
    it_should_behave_like 'convert', 'cedilla c', 'ç', '\\c{c}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented C.

describe 'String to LaTeX' do
  describe 'converts accented C letters' do
    it_should_behave_like 'convert', 'acute c', 'ć', "\\'{c}"
    it_should_behave_like 'convert', 'check c', 'č', '\\v{c}'
    it_should_behave_like 'convert', 'cedilla C', 'Ç', '\\c{C}'
    it_should_behave_like 'convert', 'acute C', 'Ć', "\\'{C}"
    it_should_behave_like 'convert', 'check C', 'Č', '\\v{C}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented D.

describe 'String to LaTeX' do
  describe 'converts accented D letters' do
    it_should_behave_like 'convert', 'check D', 'ď', '\\v{d}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented E.

describe 'String to LaTeX' do
  describe 'converts accented E letters' do
    it_should_behave_like 'convert', 'hat e', 'ê', '\\^{e}'
    it_should_behave_like 'convert', 'acute e', 'é', "\\'{e}"
    it_should_behave_like 'convert', 'umlaut e', 'ë', '\\"{e}'
    it_should_behave_like 'convert', 'grave e', 'è', '\\`{e}'
    it_should_behave_like 'convert', 'check e', 'ě', '\\v{e}'
    it_should_behave_like 'convert', 'breve e', 'ĕ', '\\u{e}'
    it_should_behave_like 'convert', 'ogonek e', 'ę', '\\k{e}'
    it_should_behave_like 'convert', 'hat E', 'Ê', '\\^{E}'
    it_should_behave_like 'convert', 'ogonek E', 'Ę', '\\k{E}'
    it_should_behave_like 'convert', 'acute E', 'É', "\\'{E}"
    it_should_behave_like 'convert', 'umlaut E', 'Ë', '\\"{E}'
    it_should_behave_like 'convert', 'grave E', 'È', '\\`{E}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented I.

describe 'String to LaTeX' do
  describe 'converts accented I letters' do
    it_should_behave_like 'convert', 'acute i', 'í', "\\'{i}"
    it_should_behave_like 'convert', 'acute I', 'Í', "\\'{I}"
    it_should_behave_like 'convert', 'hat i', 'î', '\\^{i}'
    it_should_behave_like 'convert', 'hat I', 'Î', '\\^{I}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented L.

describe 'String to LaTeX' do
  describe 'converts accented L letters' do
    it_should_behave_like 'convert', 'struck l', 'ł', '\\l'
    it_should_behave_like 'convert', 'struck L', 'Ł', '\\L'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented N.

describe 'String to LaTeX' do
  describe 'converts accented N letters' do
    it_should_behave_like 'convert', 'tilde n', 'ñ', '\\~{n}'
    it_should_behave_like 'convert', 'acute n', 'ń', "\\'{n}"
    it_should_behave_like 'convert', 'check n', 'ň', '\\v{n}'
    it_should_behave_like 'convert', 'tilde N', 'Ñ', '\\~{N}'
    it_should_behave_like 'convert', 'acute N', 'Ń', "\\'{N}"
    it_should_behave_like 'convert', 'check N', 'Ň', '\\v{N}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented O.

describe 'String to LaTeX' do
  describe 'converts accented O letters' do
    it_should_behave_like 'convert', 'hat o', 'ô', '\\^{o}'
    it_should_behave_like 'convert', 'umlaut o', 'ö', '\\"{o}'
    it_should_behave_like 'convert', 'acute o', 'ó', "\\'{o}"
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented R.

describe 'String to LaTeX' do
  describe 'converts accented R letters' do
    it_should_behave_like 'convert', 'check r', 'ř', '\\v{r}'
    it_should_behave_like 'convert', 'check R', 'Ř', '\\v{R}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented S.

describe 'String to LaTeX' do
  describe 'converts accented S letters' do
    it_should_behave_like 'convert', 'cedilla s', 'ş', '\\c{s}'
    it_should_behave_like 'convert', 'check s', 'š', '\\v{s}'
    it_should_behave_like 'convert', 'comma s', 'ș', '\\,{s}'
    it_should_behave_like 'convert', 'cedilla S', 'Ş', '\\c{S}'
    it_should_behave_like 'convert', 'check S', 'Š', '\\v{S}'
    it_should_behave_like 'convert', 'comma S', 'Ș', '\\,{S}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented T.

describe 'String to LaTeX' do
  describe 'converts accented T letters' do
    it_should_behave_like 'convert', 'comma t', 'ț', '\\,{t}'
    it_should_behave_like 'convert', 'check t', 'ť', '\\v{t}'
    it_should_behave_like 'convert', 'cedilla t', 'ţ', '\\c{t}'
    it_should_behave_like 'convert', 'check T', 'Ť', '\\v{T}'
    it_should_behave_like 'convert', 'comma T', 'Ț', '\\,{T}'
    it_should_behave_like 'convert', 'cedilla T', 'Ţ', '\\c{T}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented U.

describe 'String to LaTeX' do
  describe 'converts accented U letters' do
    it_should_behave_like 'convert', 'acute u', 'ú', "\\'{u}"
    it_should_behave_like 'convert', 'umlaut u', 'ü', '\\"{u}'
    it_should_behave_like 'convert', 'ring u', 'ů', '\\r{u}'
    it_should_behave_like 'convert', 'acute U', 'Ú', "\\'{U}"
    it_should_behave_like 'convert', 'umlaut U', 'Ü', '\\"{U}'
    it_should_behave_like 'convert', 'ring U', 'Ů', '\\r{U}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented Y.

describe 'String to LaTeX' do
  describe 'converts accented Y letters' do
    it_should_behave_like 'convert', 'acute y', 'ý', "\\'{y}"
    it_should_behave_like 'convert', 'acute Y', 'Ý', "\\'{Y}"
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented Z.

describe 'String to LaTeX' do
  describe 'converts accented Z letters' do
    it_should_behave_like 'convert', 'check z', 'ž', '\\v{z}'
    it_should_behave_like 'convert', 'acute z', 'ź', "\\'{z}"
    it_should_behave_like 'convert', 'acute Z', 'Ź', "\\'{Z}"
    it_should_behave_like 'convert', 'dot z', 'ż', '\\.{z}'
    it_should_behave_like 'convert', 'dot Z', 'Ż', '\\.{Z}'
    it_should_behave_like 'convert', 'check Z', 'Ž', '\\v{Z}'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Accented doubles.

describe 'String to LaTeX' do
  describe 'converts accented chars' do
    it_should_behave_like 'convert', 'accent chars AE', 'Æ', '\\AE'
    it_should_behave_like 'convert', 'accent chars ae', 'æ', '\\ae'
    it_should_behave_like 'convert', 'accent chars OE', 'Œ', '\\OE'
    it_should_behave_like 'convert', 'accent chars oe', 'œ', '\\oe'
    it_should_behave_like 'convert', 'accent chars O', 'Ø', '\\O'
    it_should_behave_like 'convert', 'accent chars o', 'ø', '\\o'
    it_should_behave_like 'convert', 'accent chars DH', 'Ð', '\\DH'
    it_should_behave_like 'convert', 'accent chars dh', 'ð', '\\dh'
    it_should_behave_like 'convert', 'accent chars dj', 'đ', '\\dj'
    it_should_behave_like 'convert', 'accent chars NG', 'Ŋ', '\\NG'
    it_should_behave_like 'convert', 'accent chars ng', 'ŋ', '\\ng'
    it_should_behave_like 'convert', 'accent chars L', 'Ł', '\\L'
    it_should_behave_like 'convert', 'accent chars ss', 'ß', '\\ss'
    it_should_behave_like 'convert', 'accent chars ss', 'ẞ', '\\SS'
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ Convert tests: Normal and punctuation.

describe 'String to LaTeX' do
  # normal characters
  it 'does not convert the normal chars to LaTeX syntax' do
    check_conversion(
      'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',
      'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    )
  end
  # punctuation marks not previously covered.
  it 'does not convert the normal punctuation to LaTeX symbols' do
    check_conversion(
      ',.?!@*()-+=""/',
      ',.?!@*()-+=""/'
    )
  end
end

# -------------------------------------------------------------------------- }}}
