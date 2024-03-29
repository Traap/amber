# frozen_string_literal: true

# {{{ TestEvidence module documentation.
#
# Test Output Factory is the files amber creats when running Test Suites,
# Test Plans, Test Cases, and Test Steps.  The output factory has two formats:
#
# 1) Without Browers and Languages
#   test-output/
#     test-results.tex
#     requirements.csv
#     factory/
#       plan/
#       suite/
#       case/
#         about/
#           about.tex
#           about-step-001-log.tex
#           about-step-001-status.tex
#
# 2) With Browers and Languages
#   test-output/
#     test-results.tex     requirements.csv
#     chrome/
#       fr/
#         factory/
#           plan/
#           suite/
#           case/
#             about/
#               about.tex
#               about-step-001-log.tex
#               about-step-001-status.tex
#               about-001-001.png
#               about-001-001.csv
#               about-001-002.png
#               about-001-002.csv
#
# Amber creates either LaTeX (tex) or Ascii (txt) files.  png and csv files are
# created by the program Amber invokes.
#
# -------------------------------------------------------------------------- }}}
# {{{ Required files

require 'fileutils'

# -------------------------------------------------------------------------- }}}

module Amber
  # rubocop:disable Metrics.ModuleLength
  module TestEvidence
    # rubocop:enable Metrics.ModuleLength
    # {{{ Definitions.

    TEST_OUTPUT_DIR = 'test-output'
    TEST_OUTPUT = TestEvidence::TEST_OUTPUT_DIR + File::SEPARATOR
    RESULT_FILE_EXTENSION = '.txt'
    STEP_FILE = '-step-'
    STEP_LOG = '-log'
    STEP_STATUS = '-status'
    ENVIRONMENT_LOG = "#{TestEvidence::TEST_OUTPUT}environment"
    TEST_RESULTS_LOG = "#{TestEvidence::TEST_OUTPUT}test-results"
    LATEX_FILE_EXTENSION = '.tex'
    ASCII_FILE_EXTENSION = '.txt'
    REQUIREMENTS_LOG = "#{TestEvidence::TEST_OUTPUT}requirements.csv"
    COMMAND_LOG = "#{TestEvidence::TEST_OUTPUT}commands.log"

    # ---------------------------------------------------------------------- }}}
    # {{{ obliterate_test_output

    def self.obliterate_test_output
      FileUtils.remove_dir(TestEvidence::TEST_OUTPUT_DIR, true)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ assemble_test_output_root

    def self.assemble_test_output_root(options)
      if options.browser? && options.language?
        TestEvidence::TEST_OUTPUT +
          options.browser + File::SEPARATOR +
          Amber::Language::CODE.key(options.language) + File::SEPARATOR
      else
        TestEvidence::TEST_OUTPUT
      end
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ assemble_temp_root

    def self.assemble_temp_root(options)
      "#{TestEvidence.assemble_test_output_root(options)}tmp"
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ create_directory_when_needed

    def self.create_directory_when_needed(dir)
      FileUtils.mkdir_p dir
    rescue StandardError
      msg = "Could not create: #{dir}"
      abort msg
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ open_files

    def self.open_file(file)
      TestEvidence.create_directory_when_needed(File.dirname(file))
      File.open(file, 'a')
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ use_file_extension

    def self.use_file_extension(options)
      # rubocop:disable Style/MultilineTernaryOperator

      options.writer == 'LaTeX' ? TestEvidence::LATEX_FILE_EXTENSION
                                : TestEvidence::ASCII_FILE_EXTENSION

      # rubocop:enable Style/MultilineTernaryOperator
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ open_log_file

    def self.open_log_file(input, options)
      TestEvidence.open_file(
        TestEvidence.assemble_test_output_root(options) +
        File.dirname(input) +
        File::SEPARATOR +
        File.basename(input, '.*') +
        TestEvidence.use_file_extension(options)
      )
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ open_environment_log_file.

    def self.open_environment_log_file(options)
      TestEvidence.open_file(
        TestEvidence::ENVIRONMENT_LOG +
        TestEvidence.use_file_extension(options)
      )
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ record_test_name

    def self.record_test_name(name, options)
      handle =
        TestEvidence.open_file(
          TestEvidence::TEST_RESULTS_LOG +
          TestEvidence.use_file_extension(options)
        )
      handle.write(name)
      TestEvidence.close_file(handle)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ get_test_case_log

    def self.get_test_case_log(input, nbr, options)
      TestEvidence.assemble_test_output_root(options) +
        File.dirname(input) +
        File::SEPARATOR +
        File.basename(input, '.*') +
        TestEvidence::STEP_FILE +
        nbr.to_s.rjust(3, '0') +
        TestEvidence::STEP_LOG +
        TestEvidence.use_file_extension(options)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ record_test_case_status

    # rubocop:disable Metrics.MethodLength
    def self.record_test_case_status(input, nbr, test_result, options)
      handle =
        TestEvidence.open_file(
          TestEvidence.assemble_test_output_root(options) +
          File.dirname(input) +
          File::SEPARATOR +
          File.basename(input, '.*') +
          TestEvidence::STEP_FILE +
          nbr.to_s.rjust(3, '0') +
          TestEvidence::STEP_STATUS +
          TestEvidence.use_file_extension(options)
        )
      handle.write(test_result)
      TestEvidence.close_file(handle)
    end
    # rubocop:enable Metrics.MethodLength

    # ---------------------------------------------------------------------- }}}
    # {{{ close_file

    def self.close_file(handle)
      handle.close
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ run_from_temp_directory

    def self.run_from_temp_directory(command, workingdir)
      pwd = Dir.getwd
      TestEvidence.create_directory_when_needed workingdir
      Dir.chdir workingdir
      stdout, stderr, status = Open3.capture3 command
      Dir.chdir pwd
      [stdout, stderr, status]
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ record_requirement_tested

    def self.record_requirement_tested(name, requirements)
      reqs = Amber::Requirement.to_array(requirements)
      return if reqs.nil?

      skip_header = File.file?(TestEvidence::REQUIREMENTS_LOG)
      handle = TestEvidence.open_file(TestEvidence::REQUIREMENTS_LOG)
      handle.write "requirement | test\n" unless skip_header
      reqs.each do |req|
        handle.write("#{req} | #{name}\n")
      end
      TestEvidence.close_file(handle)
    end

    # ---------------------------------------------------------------------- }}}
    # {{{ record_amber_command

    def self.record_amber_command(name)
      handle = TestEvidence.open_file(TestEvidence::COMMAND_LOG)
      handle.write("#{name}\n")
      TestEvidence.close_file(handle)
    end

    # ---------------------------------------------------------------------- }}}
  end
end
