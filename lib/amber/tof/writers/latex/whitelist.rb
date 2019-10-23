# frozen_string_literal: true

module Amber
  module LaTeXWhiteList
    NAMES = [
      '\\begin{enumerate}',
      '\\end{enumerate}',
      '\\begin{itemize}',
      '\\end{itemize}',
      '\\item', 
      '\\LaTeX.',
      '\\LaTeX\\'
    ] 
  end
end
