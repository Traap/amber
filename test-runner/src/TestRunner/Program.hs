module TestRunner.Program (Program(..)) where

-- Type: Program
data Program = PROGRAM
                 {p :: String     -- Command to run.
                 ,a :: [String]   -- Arguments to pass to command.
                 } deriving (Show)

