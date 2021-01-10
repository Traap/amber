 # frozen_string_literal:true

require 'amber'

shared_examples 'whitelist' do |input, condition|
  context "#{input}" do
    challenge = Amber::LaTeXWhiteList::NAMES.include? input
    if condition 
      message = "should be found"
    else
      message = "should not be found"
    end 
    it message do
      expect(challenge).to be(condition)
    end 
  end
end

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
