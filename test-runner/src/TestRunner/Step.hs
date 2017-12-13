module TestRunner.Step (Step(..)) where

-- Type:  Recipe Step.
data Step = STEP
  {n :: Int             -- Step Number
  ,r :: String          -- Requirement
  ,c :: String          -- Confirm this about the requirement.
  ,x :: String          -- Expected test result.
  ,p :: String          -- Command to run.
  ,a :: [String]        -- Arguments to pass to command.
--  ,f :: Maybe [String]  -- Include these files in the test report.
  ,f :: [String]  -- Include these files in the test report.
  } deriving (Show)
