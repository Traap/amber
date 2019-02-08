# LaTeX_Utility assembles the strings listed below which represent macros
# autodoc expands into the locations of files created by amber.
#
# (tpo) Test Plan Output
# (tso) Test Suite Output
# (tco) Test Case Output
#
# When browser and language are present:
# Note C:  three parameters.  C is third letter of English alphabet.
#    \tpoC{Chrome}{en}{full-path-to/factory/plan/About/About.tex}
#    \tsoC{Chrome}{en}{full-path-to/factory/suite/About/About.tex}
#    \tcoC{Chrome}{en}{full-path-to/factory/case/About/About.tex}
#
# When browser and language are NOT present:
#    \tpo{full-path-to/factory/plan/About/About.tex}
#    \tso{full-path-to/factory/suite/About/About.tex}
#    \tco{full-path-to/factory/case/About/About.tex}
#
# ------------------------------------------------------------------------------
require 'amber/cli/language'
require 'amber/cli/options'
require 'amber/tof/writers/latex/test'

module Amber
  module LaTeXUtility

    # --------------------------------------------------------------------------

    def self.get_plan_macro(decoratee)
      macro = ""
      macro << '\\tpo' \
            << LaTeXUtility.append_browser_and_language(decoratee) \
            << LaTeXUtility.append_filename(decoratee)
    end

    # --------------------------------------------------------------------------

    def self.get_suite_macro(decoratee)
      macro = ""
      macro << '\\tso' \
            << LaTeXUtility.append_browser_and_language(decoratee) \
            << LaTeXUtility.append_filename(decoratee)
    end

    # --------------------------------------------------------------------------

    def self.get_case_macro(decoratee)
      macro = ""
      macro << '\\tco' \
            << LaTeXUtility.append_browser_and_language(decoratee) \
            << LaTeXUtility.append_filename(decoratee)
    end

    # --------------------------------------------------------------------------

    def self.gather_browser_and_language(decoratee)
      if decoratee.options.has_browser? && decoratee.options.has_language?
        browser  = decoratee.options.browser
        language = decoratee.options.language
        code     = Amber::Language::CODE.key(language)
        [browser, code]
      end
    end

    # --------------------------------------------------------------------------

    # def self.append_browser_and_language(macro, decoratee)
    def self.append_browser_and_language(decoratee)
      browser, code = Amber::LaTeXUtility.gather_browser_and_language(decoratee)
      macro = ""
      unless browser.nil? && code.nil? then
        macro << 'C{' << browser << '}{' << code << '}'
      end
      macro
    end

    # --------------------------------------------------------------------------

    # def self.append_filename(macro, decoratee)
    def self.append_filename(decoratee)
      pwd = FileUtils.pwd()
      browser, code = Amber::LaTeXUtility.gather_browser_and_language(decoratee)
      macro = ""

      macro << '{' \
            << pwd << File::SEPARATOR \
            << TestEvidence::TEST_OUTPUT_DIR << File::SEPARATOR

      macro << "#{browser}" << File::SEPARATOR unless browser.nil?

      macro << "#{code}" << File::SEPARATOR unless code.nil? 

      macro << File.dirname(decoratee.filename) << File::SEPARATOR \
            << File.basename(decoratee.filename, '.*') \
            << '}' \
            << "\n"
    end

    # --------------------------------------------------------------------------

  end
end
