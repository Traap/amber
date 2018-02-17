require 'fileutils'

module Amber
  module TestEvidence 
    Test_Output = "test-output" + File::SEPARATOR
    Log_File_Extension = ".log"
    Result_File_Extension = ".txt"

    def TestEvidence.create_directory_when_needed(dir)
      FileUtils::mkdir_p dir
    end

    def TestEvidence.open_file(file)
      puts "TestEvidence.openfile #{file}"
      TestEvidence.create_directory_when_needed(File.dirname(file))
      File.open(file, 'a')
    end
  
    def TestEvidence.open_log_file(input)
      puts "TestEvidence.open_log_file #{input}"
      file = TestEvidence::Test_Output + 
             File.dirname(input) +
             File::SEPARATOR + 
             File.basename(input, ".*") + 
             TestEvidence::Log_File_Extension

      TestEvidence.open_file(file)
    end

    def TestEvidence.open_result_file(input)
      file = TestEvidence::Test_Output + 
             File.dirname(input) +
             File::SEPARATOR + 
             File.basename(input, ".*") + 
             TestEvidence::Result_File_Extension

      TestEvidence.open_file(file)
    end

    def TestEvidence.open_step_result_file(input, nbr)
      file = TestEvidence::Test_Output + 
             File.dirname(input) +
             File::SEPARATOR + 
             File.basename(input, ".*") + 
             Step_File, nbr, 
             TestEvidence::Result_File_Extension

      TestEvidence.open_file(file)
    end

    def TestEvidence.close_file(handle)
      handle.close
    end
    
  end # TestEvidence 
end # Amber
