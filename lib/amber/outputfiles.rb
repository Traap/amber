require 'fileutils'

module Amber
  module TestEvidence

    Test_Output_Dir = "test-output"
    Test_Output = TestEvidence::Test_Output_Dir + File::SEPARATOR
    Temp_Dir = TestEvidence::Test_Output + "tmp"
    Result_File_Extension = ".txt"
    Step_File = "step-"
    Environment_Log = "factory" + File::SEPARATOR + "environment"
    Test_Results_Log = "factory" + File::SEPARATOR + "test-results"
    LaTeX_File_Extension = ".tex"
    Ascii_File_Extension = ".txt"

    def TestEvidence.create_directory_when_needed(dir)
      FileUtils::mkdir_p dir
    end

    def TestEvidence.open_file(file)
      TestEvidence.create_directory_when_needed(File.dirname(file))
      File.open(file, 'a')
    end

    def TestEvidence.use_file_extension(options)
      options.writer == "LaTeX" ? TestEvidence::LaTeX_File_Extension
                                : TestEvidence::Ascii_File_Extension
    end

    def TestEvidence.open_log_file(input, options)
      TestEvidence.open_file(
             TestEvidence::Test_Output +
             File.dirname(input) +
             File::SEPARATOR +
             File.basename(input, ".*") +
             TestEvidence.use_file_extension(options)
           )
    end

    def TestEvidence.open_environment_log_file(options)
        TestEvidence.open_file(
             TestEvidence::Test_Output +
             TestEvidence::Environment_Log +
             TestEvidence.use_file_extension(options)
           )
    end

    def TestEvidence.record_test_name(name, options)
      handle =
        TestEvidence.open_file(
             TestEvidence::Test_Output +
             TestEvidence::Test_Results_Log +
             TestEvidence.use_file_extension(options)
           )
      handle.write(name)
      TestEvidence.close_file(handle)
    end

    def TestEvidence.record_final_test_result(input, nbr, test_result, options)
      handle =
        TestEvidence.open_file(
             TestEvidence::Test_Output +
             File.dirname(input) +
             File::SEPARATOR +
             TestEvidence::Step_File +
             nbr.to_s +
             TestEvidence.use_file_extension(options)
           )
      handle.write(test_result)
      TestEvidence.close_file(handle)
    end

    def TestEvidence.open_test_results(options)
      TestEvidence.open_file(
             TestEvidence::Test_Output +
             File.dirname(input) +
             File::SEPARATOR +
             TestEvidence::Test_Results_File +
             TestEvidence.use_file_extension(options)
           )
    end

    def TestEvidence.close_file(handle)
      handle.close
    end

    def TestEvidence.run_from_temp_directory(command, workingdir)
      pwd = Dir.getwd
      TestEvidence.create_directory_when_needed workingdir
      Dir.chdir workingdir
      # puts "TestEvidence.rfwd #{workingdir}"
      # puts "TestEvidence.rfwd #{command}"
      stdout, stderr, status = Open3.capture3 command
      # puts "TestEvidence.rfwd status: #{status}"
      # puts "TestEvidence.rfwd stdout: #{stdout}"
      # puts "TestEvidence.rfwd stderr: #{stderr}"
      Dir.chdir pwd
      return stdout, stderr, status
    end

  end # TestEvidence

end # Amber
