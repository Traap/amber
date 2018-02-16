require 'fileutils'

module Amber
  module TestEvidence 
    Test_Output = "test-output/"
    Log_File_Extension = ".log"
    Result_File_Extension = ".txt"
    Step_File = "/step-"

    def TestEvidence.create_directory_when_needed(dir)
      FileUtils::mkdir_p dir
    end

    def TestEvidence.open_file(file)
      TestEvidence.create_directory_when_needed(File.dirname(file))
      File.open(file, 'a')
    end
  
    def TestEvidence.open_log_file(input)
      file = TestEvidence::Test_Output + 
             File.dirname(input[0]) +
             File.basename(input[0], ".*") + 
             TestEvidence::Log_File_Extension

      TestEvidence.open_file(file)
    end

    def TestEvidence.open_result_file(input)
      file = TestEvidence::Test_Output + 
             File.dirname(input[0]) +
             File.basename(input[0], ".*") + 
             TestEvidence::Result_File_Extension

      TestEvidence.open_file(file)
    end

    def TestEvidence.open_step_log_file(input, nbr)
      file = TestEvidence::Test_Output + 
             File.dirname(input[0]) +
             File.basename(input[0], ".*") + 
             TestEvidence::Log_File_Extension

      TestEvidence.open_file(file)
    end

    def TestEvidence.open_step_result_file(input, nbr)
      file = TestEvidence::Test_Output + 
             File.dirname(input[0]) +
             File.basename(input[0], ".*") + 
             Step_File, nbr, 
             TestEvidence::Result_File_Extension

      TestEvidence.open_file(file)
    end

    def TestEvidence.close_file(handle)
      handle.close
    end
    
  end # TestEvidence 
end # Amber
