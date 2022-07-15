# frozen_string_literal: true

# {{{ LaTeX_Utility assembles the strings listed below which represent macros
#     autodoc expands into the locations of files created by amber.
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
# -------------------------------------------------------------------------- }}}
# {{{ Required files.

require 'amber/cli/language'
require 'amber/cli/options'
require 'amber/tof/writers/latex/test'

# -------------------------------------------------------------------------- }}}

module Amber
  module LaTeXUtility
    # {{{ get_plan_macro

    def self.get_plan_macro(decoratee)
      # Return a string as a LaTeX macro.
      "\\tpo\
        #{LaTeXUtility.append_browser_and_language(decoratee)}\
        #{LaTeXUtility.append_filename(decoratee)}".gsub!(' ', '')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_suite_macro

    def self.get_suite_macro(decoratee)
      # Return a string as a LaTeX macro.
      "\\tso\
        #{LaTeXUtility.append_browser_and_language(decoratee)}\
        #{LaTeXUtility.append_filename(decoratee)}".gsub!(' ', '')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_case_macro

    def self.get_case_macro(decoratee)
      # Return a string as a LaTeX macro.
      "\\tco\
        #{LaTeXUtility.append_browser_and_language(decoratee)}\
        #{LaTeXUtility.append_filename(decoratee)}".gsub!(' ', '')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ gather_browser_and_language

    def self.gather_browser_and_language(decoratee)
      return unless decoratee.options.browser? && decoratee.options.language?

      browser  = decoratee.options.browser
      language = decoratee.options.language
      code     = Amber::Language::CODE.key(language)
      [browser, code]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ append_browser_and_language

    def self.append_browser_and_language(decoratee)
      browser, code = Amber::LaTeXUtility.gather_browser_and_language(decoratee)

      # Return a string as a LaTeX macro.
      "C{#{browser}}{#{code}}".gsub(' ', '') unless browser.nil? && code.nil?
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ append_filename

    def self.append_filename(decoratee)
      pwd = FileUtils.pwd()
      browser, code = Amber::LaTeXUtility.gather_browser_and_language(decoratee)

      # macro = ''
      # macro << '{' \
      #       << pwd << File::SEPARATOR \
      #       << TestEvidence::TEST_OUTPUT_DIR << File::SEPARATOR
      # macro << browser.to_s << File::SEPARATOR unless browser.nil?
      # macro << code.to_s << File::SEPARATOR unless code.nil?
      # macro << File.dirname(decoratee.filename) << File::SEPARATOR \
      #       << File.basename(decoratee.filename, '.*') \
      #       << '}' \
      #       << "\n"

      # Return a string repressenting full path to a file.
      if browser.nil? && code.nil?
        "{\
          #{pwd}#{File::SEPARATOR}\
          #{TestEvidence::TEST_OUTPUT_DIR}#{File::SEPARATOR}\
          #{File.dirname(decoratee.filename)}#{File::SEPARATOR}\
          #{File.basename(decoratee.filename, '.*')}\
        }\n".gsub(' ', '')
      else
        "{\
          #{pwd}#{File::SEPARATOR}\
          #{TestEvidence::TEST_OUTPUT_DIR}#{File::SEPARATOR}\
          #{browser}#{File::SEPARATOR}\
          #{code}#{File::SEPARATOR}\
          #{File.dirname(decoratee.filename)}#{File::SEPARATOR}\
          #{File.basename(decoratee.filename, '.*')}\
        }\n".gsub(' ', '')
      end
    end

    # ---------------------------------------------------------------------- }}}
  end
end
