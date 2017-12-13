module TestRunner.Environment
  ( recordEquipment
  ) where

import Data.List
import Data.Text
import System.Directory
import System.Environment
import System.IO
import System.Info
import TestRunner.Latex
import TestRunner.OutputFiles
import TestRunner.PrettyPrint
import System.Process(readProcess)

-------------------------------------------------------------------------------
-- The following functions create a LaTex listing describing the operating
-- system environment.
-------------------------------------------------------------------------------
osx :: String
osx = "         OS Name: OSX"

lin :: String
lin = "         OS Name: Linux"

win :: String
win = "         OS Name: Windows"

unk :: String
unk = "         OS Name: "

-- | record the system environment.
recordEquipment :: String -> IOMode -> IO ()
recordEquipment fpath fmode = do
  h <- openEquipmentFile fpath fmode

  hPutStrLn h beginVerbatim
  hPutStrLn h "\nSystem Environment"
  hPutStrLn h $ "Operating System: " ++ os
  case os of
    "darwin"  -> hPutStrLn h osx
    "linux"   -> hPutStrLn h lin
    "mingw32" -> hPutStrLn h win
    _         -> hPutStrLn h $ unk ++ os
  hPutStrLn h $ "    Architecture: " ++ arch
  hPutStrLn h $ "   Compiler Name: " ++ compilerName
  home <- getHomeDirectory
  hPutStrLn h $ "  Home Directory: " ++ home

  sha<-readProcess "git" ["rev-parse","HEAD"] ""
  hPutStrLn h $ "\nGit SHA: " ++ sha
  hPutStrLn h "\nEnvironment Variables:"
  env <- getEnvironment >>= (return.sort)
  mapM_ (recordEnv h) env
  hPutStrLn h endVerbatim
  hClose h

-- | record the environment tuple.
recordEnv :: Handle -> (String, String) -> IO ()
recordEnv h (key, value) = do
   let v = unpack . Data.Text.unwords . split (==':') $ pack value
   let s  = key ++ ": " ++ v
   ppSout h s
