# frozen_string_literal: true

# The types of output Writers.  The default is LaTeX.
module Amber
  module Writer
    NAMES = %w[Ascii LaTeX].freeze
    DEFAULT = 'LaTeX'
  end
end
