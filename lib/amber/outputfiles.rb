# Test Output Factory the file system amber creats when running Test Suites,
# Test Plans, Test Cases, and Test Steps.  The output factor has two formats:
#
# 1) Without Browers and Languages
#   test-output/
#     factory/
#       plan/
#       suite/
#       case/
#         about/
#           about.tex
#           step-001.tex
#
# 2) With Browers and Languages
#   test-output/
#     chrome/
#       fr/
#         factory/
#           plan/
#           suite/
#           case/
#             about/
#               about.tex
#               step-001.tex
#               about-001.png
#               about-001.csv
#
# Amber creates either LaTeX (tex) or Ascii (txt) files.  png and csv files are
# created by the program Amber invokes.
#
require 'fileutils'

module Amber
  module TestEvidence
    TEST_OUTPUT_DIR = 'test-output'.freeze
    TEST_OUTPUT = TestEvidence::TEST_OUTPUT_DIR + File::SEPARATOR
    RESULT_FILE_EXTENSION = '.txt'.freeze
    STEP_FILE = 'step-'.freeze
    ENVIRONMENT_LOG = TestEvidence::TEST_OUTPUT + 'environment'
    TEST_RESULTS_LOG = TestEvidence::TEST_OUTPUT + 'test-results'
    LATEX_FILE_EXTENSION = '.tex'.freeze
    ASCII_FILE_EXTENSION = '.txt'.freeze

    # --------------------------------------------------------------------------

    def self.obliterate_test_output(options)
      FileUtils.remove_dir(TestEvidence::TEST_OUTPUT_DIR, true)
    end

    # --------------------------------------------------------------------------

    def self.assemble_test_output_root(options)
      if options.has_browser? && options.has_language?
        TestEvidence::TEST_OUTPUT +
          options.browser + File::SEPARATOR +
          Amber::Language::CODE.key(options.language) + File::SEPARATOR
      else
        TestEvidence::TEST_OUTPUT
      end
    end

    # --------------------------------------------------------------------------

    def self.assemble_temp_root(options)
      TestEvidence.assemble_test_output_root(options) + 'tmp'
    end

    # --------------------------------------------------------------------------

    def self.create_directory_when_needed(dir)
      FileUtils.mkdir_p dir
    end

    # --------------------------------------------------------------------------

    def self.open_file(file)
      TestEvidence.create_directory_when_needed(File.dirname(file))
      File.open(file, 'a')
    end

    # --------------------------------------------------------------------------

    def self.use_file_extension(options)
      # rubocop:disable Style/MultilineTernaryOperator

      options.writer == 'LaTeX' ? TestEvidence::LATEX_FILE_EXTENSION
                                : TestEvidence::ASCII_FILE_EXTENSION

      # rubocop:enable Style/MultilineTernaryOperator
    end

    # --------------------------------------------------------------------------

    def self.open_log_file(input, options)
      TestEvidence.open_file(
        TestEvidence.assemble_test_output_root(options) +
        File.dirname(input) +
        File::SEPARATOR +
        File.basename(input, '.*') +
        TestEvidence.use_file_extension(options)
      )
    end

    # --------------------------------------------------------------------------

    def self.open_environment_log_file(options)
      TestEvidence.open_file(
        TestEvidence::ENVIRONMENT_LOG +
        TestEvidence.use_file_extension(options)
      )
    end

    # --------------------------------------------------------------------------

    def self.record_test_name(name, options)
      handle =
        TestEvidence.open_file(
          TestEvidence::TEST_RESULTS_LOG +
          TestEvidence.use_file_extension(options)
        )
      handle.write(name)
      TestEvidence.close_file(handle)
    end

    # --------------------------------------------------------------------------

    def self.record_final_test_result(input, nbr, test_result, options)
      handle =
        TestEvidence.open_file(
          TestEvidence.assemble_test_output_root(options) +
          File.dirname(input) +
          File::SEPARATOR +
          TestEvidence::STEP_FILE +
          nbr.to_s.rjust(3, '0') +
          TestEvidence.use_file_extension(options)
        )
      handle.write(test_result)
      TestEvidence.close_file(handle)
    end

    # --------------------------------------------------------------------------

    def self.close_file(handle)
      handle.close
    end

    # --------------------------------------------------------------------------

    def self.run_from_temp_directory(command, workingdir)
      pwd = Dir.getwd
      TestEvidence.create_directory_when_needed workingdir
      Dir.chdir workingdir
      stdout, stderr, status = Open3.capture3 command
      Dir.chdir pwd
      [stdout, stderr, status]
    end

    # --------------------------------------------------------------------------
  end
end
