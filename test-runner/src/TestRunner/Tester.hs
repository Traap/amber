module TestRunner.Tester
  ( runProtocols
  , runRecipe
  ) where

import System.Exit
import System.IO
import System.Process
import TestRunner.Directory
import TestRunner.OutputFiles
import TestRunner.RecipeResult
import TestRunner.RecipeStep
import TestRunner.RecipeTable
import TestRunner.Protocol
import TestRunner.Step

-- | Create output directories and run protocols.
runProtocols :: [Protocol] -> [String] -> IO ()
runProtocols protocols directories = do
  mapM_ (ensureDirectoryExists CLEAN) directories
  mapM_ performAction protocols
  return ()

-- | Save the current directory, perform a monadic action, and restore
-- the working directory.
performAction :: Protocol -> IO ()
performAction protocol = resetDirectoryAfter $ do
  putStrLn (msg protocol)
  _ <- (pn protocol)
  return ()

-- | Run recipe steps.
runRecipe :: [Step] -> String -> IOMode -> IO ()
runRecipe steps fpath fmode = do
  h <- openResultsFile fpath fmode
  excodes <- mapM (runStep h fpath) steps
  hPutRecipeResults h excodes fpath
  hClose h
  putRecipeTable steps fpath fmode
  return ()

-- | Run a system command p with arguments a capturing exitCode and standard output.
runStep :: Handle -> String -> Step -> IO ExitCode
runStep h fpath step = do
  (excode, sout, serr) <- readProcessWithExitCode (p step) (a step) []
  hPutStepResults h step excode (sout++serr) fpath
  return excode
