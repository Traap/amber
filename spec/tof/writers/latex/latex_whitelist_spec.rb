 # frozen_string_literal:true

require 'rspec'
require 'amber'

shared_examples 'lookup' do |input, condition|
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
  it_behaves_like 'lookup', '\\begin{enumerate}', true
  it_behaves_like 'lookup', '\\end{enumerate}', true

  it_behaves_like 'lookup', '\\begin{itemize}', true
  it_behaves_like 'lookup', '\\end{itemize}', true

  it_behaves_like 'lookup', '\\item', true

  it_behaves_like 'lookup', '\\begin{document}', false
  it_behaves_like 'lookup', '\\end{document}', false

  it_behaves_like 'lookup', '{page}', false

  it_behaves_like 'lookup', '\\LaTeX.', true 
  it_behaves_like 'lookup', '\\latex', false
  it_behaves_like 'lookup', '\\LaTeX\\', true 
end
