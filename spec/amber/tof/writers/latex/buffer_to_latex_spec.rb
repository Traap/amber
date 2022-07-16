# frozen_string_literal:true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ LaTEx Buffer Shared examples.

shared_examples 'buffer' do |description, input, output|
  context "#{description}\n\tfrom #{input} \n\t  to #{output}" do
    subject { Amber::StringToLaTeX.convert(input) }
    it { should eq(output) }
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ LaTeX Buffer tests.

describe 'Amber LaTeX Buffer' do
  it_should_behave_like \
    'buffer', \
    'enumerate', \
    '\\begin{enumerate} \\item {one} \\item {two} \\end{enumerate}', \
    '\begin{enumerate} \item {one} \item {two} \end{enumerate}'
  it_should_behave_like \
    'buffer', \
    'itemize', \
    '\\begin{itemize} \\item {one} \\item {two} \\end{itemize}', \
    '\begin{itemize} \item {one} \item {two} \end{itemize}'

  it_should_behave_like \
    'buffer', \
    'cost sample', \
    'Option #1 & is worth $1.12', \
    'Option \\#1 & is worth \\$1.12'

  it_should_behave_like \
    'buffer', \
    'LaTeX', \
    '\\LaTeX\\ is correct \latex is not correct', \
    '\LaTeX\ is correct \latex is not correct'

  it_should_behave_like \
    'buffer', \
    'end with LaTeX', \
    'This sentence ends with \\LaTeX.', \
    'This sentence ends with \LaTeX.'
end

# --------------------------------------------------------------------------
# }}}
