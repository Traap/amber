# frozen_string_literal:true

# {{{ Required files.

require 'amber'

# -------------------------------------------------------------------------- }}}
# {{{ LaTeX whitelist shared examples.

shared_examples 'whitelist' do |input, condition|
  context input.to_s do
    challenge = Amber::LaTeXWhiteList::NAMES.include? input
    message = if condition
                'should be found'
              else
                'should not be found'
              end
    it message do
      expect(challenge).to be(condition)
    end
  end
end

# -------------------------------------------------------------------------- }}}
# {{{ LaTeX whitelist shared tests.

describe 'Amber LaTeX Whitelist' do
  it_behaves_like 'whitelist', '\\begin{enumerate}', true
  it_behaves_like 'whitelist', '\\end{enumerate}', true

  it_behaves_like 'whitelist', '\\begin{itemize}', true
  it_behaves_like 'whitelist', '\\end{itemize}', true

  it_behaves_like 'whitelist', '\\item', true

  it_behaves_like 'whitelist', '\\begin{document}', false
  it_behaves_like 'whitelist', '\\end{document}', false

  it_behaves_like 'whitelist', '{page}', false

  it_behaves_like 'whitelist', '\\LaTeX.', true
  it_behaves_like 'whitelist', '\\latex', false
  it_behaves_like 'whitelist', '\\LaTeX\\', true
end

# -------------------------------------------------------------------------- }}}
