module TestRunner.RecipeStep
  ( hPutStepResults
  ) where

import Data.List
import System.Exit
import System.IO
import TestRunner.Latex
import TestRunner.OutputFiles
import TestRunner.PrettyPrint
import TestRunner.Step

-------------------------------------------------------------------------------
-- The following functions write LaTex output containing human-readable text
-- indicating test step, requirement, what to confirm, what the expectations are
-- the command and arguments used, the test result status PASS or FAIL,
-- evidence written to the console, and graphical pictures if they were created.
-------------------------------------------------------------------------------

-- | Record Step results to standard output.
hPutStepResults :: Handle -> Step -> ExitCode -> String -> String -> IO ()
hPutStepResults h step excode sout fpath = do
  hPutStrLn h beginVerbatim
  hPutStepDetail h step excode fpath
  hPutConsoleEvidence h sout
  hPutGraphicalEvidence h step
  hPutStrLn h endVerbatim

-- | Record Step detail to standard output.
hPutStepDetail :: Handle -> Step -> ExitCode -> String -> IO ()
hPutStepDetail h step excode fpath = do
  hPutStrLn h $ "  Test Step: " ++ (show (n step))
  hPutStrLn h $ "Requirement: " ++ (r step)
  hPutStr   h $ "    Confirm: "
  _ <- ppSout h (c step)
  hPutStr   h $ "Expectation: "
  _ <- ppSout h (x step)
  hPutStrLn h $ "    Command: " ++ (p step) ++ " " ++ intercalate " " (a step)
  hPutStepStatus h step excode fpath

-- ! Record step status pass or fail.
hPutStepStatus :: Handle -> Step -> ExitCode -> String -> IO ()
hPutStepStatus h step excode fpath = do
  case excode of
    ExitSuccess -> do
      hPutStrLn h "Test Result: PASS"
      putStepStatus step fpath "PASS"
    ExitFailure _ -> do
      hPutStrLn h "Test Result: FAIL"
      putStepStatus step fpath "FAIL"
      hPutStrLn h $ " Error Code: " ++ (show excode)

-- | Record any console output as test evidence.
hPutConsoleEvidence :: Handle -> String -> IO ()
hPutConsoleEvidence h sout = do
  case null sout of
    True  ->
      hPutStrLn h "   Evidence: n/a\n"
    False -> do
      hPutStrLn h "   Evidence: Starts on next line.\n"
      ppSout h sout

-- | Record any graphics generated as test evidence.
hPutGraphicalEvidence :: Handle -> Step -> IO ()
hPutGraphicalEvidence h step = do
  case (length (f step) > 0) of
    False -> return ()
    True  -> do
      hPutStrLn h $ "\\begin{figure}"
      mapM_ (hPutGraphicalLink h) (f step)
      hPutStrLn h $ "\\end{figure}"

-- | Record graphic link
hPutGraphicalLink :: Handle -> String -> IO ()
hPutGraphicalLink h fpath = do
  hPutStrLn h $ "\\includegraphics[width=\\linewidth]{" ++ fpath ++ "}"
  hPutStrLn h $ "\\caption{" ++ fpath ++ "}"

-- | Write the word PASS or File to a file named step-n.tex as the step status.
putStepStatus :: Step -> String -> String -> IO ()
putStepStatus step fpath status = do
  h <- openStepResultsFile fpath WriteMode (n step)
  hPutStr h status
  hClose h

