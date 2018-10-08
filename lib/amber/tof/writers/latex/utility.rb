# LaTeX_Utility assembles the strings listed below which represent macros
# autodoc expands into the locations of files created by amber.
#
# (tpo) Test Plan Output
# (tso) Test Suite Output
# (tco) Test Case Output
#
# When browser and language are present:
# Note C:  three parameters.  C is third letter of English alphabet.
#    \tpoC{Chrome}{en}{About}
#    \tsoC{Chrome}{en}{About}
#    \tcoC{Chrome}{en}{About}
#
# When browser and language are NOT present:
#    \tpo{About}
#    \tso{About}
#    \tco{About}
#
# ------------------------------------------------------------------------------

require 'amber/cli/language'
require 'amber/cli/options'
require 'amber/tof/writers/latex/test'

module Amber
  module LaTeXUtility
    def self.get_plan_macro(decoratee)
      macro = '\\tpo'
      macro = LaTeXUtility.append_browser_and_language(macro, decoratee)
      macro = LaTeXUtility.append_filename(macro, decoratee)
    end

    def self.get_suite_macro(decoratee)
      macro = '\\tso'
      macro = LaTeXUtility.append_browser_and_language(macro, decoratee)
      macro = LaTeXUtility.append_filename(macro, decoratee)
    end

    def self.get_case_macro(decoratee)
      macro = '\\tco'
      macro = LaTeXUtility.append_browser_and_language(macro, decoratee)
      macro = LaTeXUtility.append_filename(macro, decoratee)
    end

    def self.append_browser_and_language(macro, decoratee)
      if decoratee.options.has_browser? && decoratee.options.has_language?
        browser  = decoratee.options.browser
        language = decoratee.options.language
        code     = Amber::Language::CODE.key(language)
        macro << 'C{' << browser << '}{' << code << '}'
      end

      macro
    end

    def self.append_filename(macro, decoratee)
      macro << '{' + File.basename(decoratee.filename, '.*') + "}\n"
    end
  end
end
