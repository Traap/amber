module TestRunner.RecipeResult
  ( hPutRecipeResults
  ) where

import System.Exit
import System.IO
import TestRunner.Latex
import TestRunner.OutputFiles

-------------------------------------------------------------------------------
-- The following functions write LaTex output containing human-readable text
-- indicating recipe (protocol) result PASS or FAIL.
-------------------------------------------------------------------------------


-- | record Recipe results to standard output.
hPutRecipeResults :: Handle -> [ExitCode] -> String -> IO ()
hPutRecipeResults h excodes fpath = do
  hPutStrLn h beginVerbatim
  case (all (==ExitSuccess) excodes) of
    True  -> do
      hPutStrLn h "   Protocol: PASS\n"
      putRecipeStatus 99 fpath "PASS"
    False -> do
      hPutStrLn h "   Protocol: FAIL\n"
      putRecipeStatus 99 fpath "FAIL"
  hPutStrLn h endVerbatim

-- | Record recipe result pass or fail status.
putRecipeStatus :: Int -> String -> String -> IO ()
putRecipeStatus step fpath status = do
  h <- openRecipeResultsFile fpath WriteMode step
  hPutStr h status
  hClose h

