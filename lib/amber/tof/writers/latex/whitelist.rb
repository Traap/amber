# frozen_string_literal: true

module Amber
  module LaTeXWhiteList
    NAMES = [
      '\\begin{enumerate}',
      '\\end{enumerate}',

      '\\begin{itemize}',
      '\\end{itemize}',

      '\\begin{longtable}',
      '\\end{longtable}',

      '\\begin{tabular}',
      '\\end{tabular}',

      '&',
      '{',
      '}',
      '\\',
      '|',

      '\\hline',

      '\\item',
      '\\LaTeX.',
      '\\LaTeX\\',

      '\\textbf',

      '\\newline',
      '\\tlcVspace'
    ].freeze
  end
end
