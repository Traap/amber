require 'fileutils'

module Amber
  module TestEvidence 
    Test_Output = "test-output" + File::SEPARATOR
    Log_File_Extension = ".log"
    Result_File_Extension = ".txt"
    Step_File = "step-"

    def TestEvidence.create_directory_when_needed(dir)
      FileUtils::mkdir_p dir
    end

    def TestEvidence.open_file(file)
      TestEvidence.create_directory_when_needed(File.dirname(file))
      File.open(file, 'a')
    end
  
    def TestEvidence.open_log_file(input)
      TestEvidence.open_file(
             TestEvidence::Test_Output + 
             File.dirname(input) +
             File::SEPARATOR + 
             File.basename(input, ".*") + 
             TestEvidence::Log_File_Extension
           )
    end

    def TestEvidence.record_final_test_result(input, nbr, test_result)
      handle = 
        TestEvidence.open_file(
             TestEvidence::Test_Output + 
             File.dirname(input) +
             File::SEPARATOR + 
             TestEvidence::Step_File +
             nbr.to_s + 
             TestEvidence::Result_File_Extension
           )
      handle.write(test_result)
      handle.close
    end

    def TestEvidence.close_file(handle)
      handle.close
    end
    
  end # TestEvidence 

end # Amber
