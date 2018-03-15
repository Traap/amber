require 'fileutils'

module Amber
  module TestEvidence

    Test_Output_Dir = "test-output"
    Test_Output = TestEvidence::Test_Output_Dir + File::SEPARATOR
    Result_File_Extension = ".txt"
    Step_File = "step-"
    Environment_Log = TestEvidence::Test_Output + "environment"
    Test_Results_Log = TestEvidence::Test_Output + "test-results"
    LaTeX_File_Extension = ".tex"
    Ascii_File_Extension = ".txt"

    # --------------------------------------------------------------------------

    def TestEvidence.assemble_test_output_root(options)
      if options.browser.nil? || options.language.nil?
        TestEvidence::Test_Output
      else
        TestEvidence::Test_Output +
        options.browser + File::SEPARATOR +
        Amber::Language::Code.key(options.language) + File::SEPARATOR
      end
    end

    # --------------------------------------------------------------------------

    def TestEvidence.assemble_temp_root(options)
      TestEvidence.assemble_test_output_root(options) + "tmp"
    end

    # --------------------------------------------------------------------------

    def TestEvidence.create_directory_when_needed(dir)
      FileUtils::mkdir_p dir
    end

    # --------------------------------------------------------------------------

    def TestEvidence.open_file(file)
      TestEvidence.create_directory_when_needed(File.dirname(file))
      File.open(file, 'a')
    end

    # --------------------------------------------------------------------------

    def TestEvidence.use_file_extension(options)
      options.writer == "LaTeX" ? TestEvidence::LaTeX_File_Extension
                                : TestEvidence::Ascii_File_Extension
    end

    # --------------------------------------------------------------------------

    def TestEvidence.open_log_file(input, options)
      TestEvidence.open_file(
        TestEvidence.assemble_test_output_root(options) +
        File.dirname(input) +
        File::SEPARATOR +
        File.basename(input, ".*") +
        TestEvidence.use_file_extension(options)
      )
    end

    # --------------------------------------------------------------------------

    def TestEvidence.open_environment_log_file(options)
      TestEvidence.open_file(
        TestEvidence::Environment_Log +
        TestEvidence.use_file_extension(options)
      )
    end

    # --------------------------------------------------------------------------

    def TestEvidence.record_test_name(name, options)
      handle =
        TestEvidence.open_file(
          TestEvidence::Test_Results_Log +
          TestEvidence.use_file_extension(options)
        )
      handle.write(name)
      TestEvidence.close_file(handle)
    end

    # --------------------------------------------------------------------------

    def TestEvidence.record_final_test_result(input, nbr, test_result, options)
      handle =
        TestEvidence.open_file(
          TestEvidence.assemble_test_output_root(options) +
          File.dirname(input) +
          File::SEPARATOR +
          TestEvidence::Step_File +
          nbr.to_s.rjust(3, '0') +
          TestEvidence.use_file_extension(options)
        )
      handle.write(test_result)
      TestEvidence.close_file(handle)
    end

    # --------------------------------------------------------------------------

    def TestEvidence.close_file(handle)
      handle.close
    end

    # --------------------------------------------------------------------------

    def TestEvidence.run_from_temp_directory(command, workingdir)
      pwd = Dir.getwd
      TestEvidence.create_directory_when_needed workingdir
      Dir.chdir workingdir
      stdout, stderr, status = Open3.capture3 command
      Dir.chdir pwd
      return stdout, stderr, status
    end
    
    # --------------------------------------------------------------------------

  end # TestEvidence
end # Amber
