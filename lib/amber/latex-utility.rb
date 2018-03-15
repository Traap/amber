# LaTeX_Utility assembles the strings listed below which represent macros
# autodoc expands into the locations of files created by amber.
#
# (tpo) Test Plan Output
# (tso) Test Suite Output
# (tco) Test Case Output
#
# When browser and language are present:
#    \tpo{Chrome}{en}{About}
#    \tso{Chrome}{en}{About}
#    \tco{Chrome}{en}{About}
#
# When browser and language are NOT present:
#    \tpo{About}
#    \tso{About}
#    \tco{About}
#
# ------------------------------------------------------------------------------

require 'amber/latex-test'
require 'amber/language'
require 'amber/options'

module Amber
  module LaTeX_Utility

    def LaTeX_Utility.get_plan_macro(decoratee)
      macro = "\\tpo"
      macro = LaTeX_Utility.append_browser_and_language(macro, decoratee)
      macro = LaTeX_Utility.append_filename(macro, decoratee)
    end

    def LaTeX_Utility.get_suite_macro(decoratee)
      macro = "\\tso"
      macro = LaTeX_Utility.append_browser_and_language(macro, decoratee)
      macro = LaTeX_Utility.append_filename(macro, decoratee)
    end

    def LaTeX_Utility.get_plan_macro(decoratee)
      macro = "\\tco"
      macro = LaTeX_Utility.append_browser_and_language(macro, decoratee)
      macro = LaTeX_Utility.append_filename(macro, decoratee)
    end

    def LaTeX_Utility.append_browser_and_language(macro, decoratee)
      browser  = decoratee.options.browser
      language = decoratee.options.language
      code     = Amber::Language::Code.key(language)

      if !browser.nil? && !language.nil?
        macro << "{" << browser <<  "}{" << code << "}"
      end

      return macro
    end

    def LaTeX_Utility.append_filename(macro, decoratee)
      macro << "{" + File.basename(decoratee.filename, ".*") + "}\n"
    end

  end # Writer
end # Amber
