require 'fileutils'

module Amber
  module TestEvidence 
    Test_Output = "test-output" + File::SEPARATOR
    Result_File_Extension = ".txt"
    Step_File = "step-"
    Environment_Log = "factory" + File::SEPARATOR + "environment"
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
      if options.language.nil? then
        TestEvidence.open_file(
             TestEvidence::Test_Output + 
             TestEvidence::Environment_Log +
             TestEvidence.use_file_extension(options)
           )
      else
        TestEvidence.open_file(
             TestEvidence::Test_Output + 
             "#{options.language}" + 
             File::SEPARATOR + 
             TestEvidence::Environment_Log +
             TestEvidence.use_file_extension(options)
           )
      end
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
      handle.close
    end

    def TestEvidence.close_file(handle)
      handle.close
    end
    
  end # TestEvidence 

end # Amber
