module TestRunner.Directory
  ( EnsureExistsMode(..)
  , withDirectory
  , ensureDirectoryExists
  , resetDirectoryAfter
  , moveOutputToArchiveFolder
  , safeRemoveDirectoryRecursive
  , getProjectRootPath
  , ensureDocker
  , copyOutputToArchiveFolder
  , isGitClean
  , whenGitClean
  ) where

import Data.List
import System.Exit
import System.Directory
import System.Process

--  | The EnsureExistsMode data structure enumerates the possible options
-- for how "ensureExists" deals with an existing directory.   The clean option
-- indicates that the directory will be purged, while the noclean option
-- indicates that the directory will be used as is.
data EnsureExistsMode = CLEAN | NOCLEAN

-- | Create a directory if it does not exist.   If it does exist, it can be optionally
-- purged.
ensureDirectoryExists :: EnsureExistsMode -> FilePath -> IO ()
ensureDirectoryExists CLEAN fpath = do
    safeRemoveDirectoryRecursive fpath
    ensureDirectoryExists NOCLEAN fpath
ensureDirectoryExists NOCLEAN fpath = createDirectoryIfMissing True fpath

-- | Perform some IO action but ensure that the current directory is the
-- safe afterward.
resetDirectoryAfter :: IO a -> IO a
resetDirectoryAfter action = do
    fpath <- getCurrentDirectory
    result <- action
    setCurrentDirectory fpath
    return result

-- | Perorm an IO action inside the given folder, but return the the current
-- directory afterward.
withDirectory :: FilePath -> IO a -> IO a
withDirectory fpath action =
    resetDirectoryAfter $ do
        ensureDirectoryExists NOCLEAN fpath
        setCurrentDirectory fpath
        action

-- | Recursively remove a directory if it exits.
safeRemoveDirectoryRecursive :: FilePath -> IO ()
safeRemoveDirectoryRecursive fpath = do
  b <- doesDirectoryExist fpath
  case b of
    True  -> removeDirectoryRecursive fpath
    False -> return ()

-- | Move src directory to archive folder
moveOutputToArchiveFolder :: FilePath -> IO ()
moveOutputToArchiveFolder src = do
  moveDirectory src "test-output"
  return ()

-- | Move a directory.
moveDirectory :: FilePath -> FilePath -> IO ()
moveDirectory src dst = do
  let cmd  = "mv"
  let args = ["-vf", src, dst]
  let msg  = "\t" ++ cmd ++ " " ++ intercalate " " args
  safeRemoveDirectoryRecursive dst
  (excode, _sout, _serr) <- readProcessWithExitCode cmd args []
  case (==ExitSuccess) excode of
    True -> do
        putStrLn $ msg ++ " succeeded."
        return ()
    False -> do
        putStrLn $ msg ++ " failed: " ++ (show excode)
        return ()

getProjectRootPath :: IO FilePath
getProjectRootPath = do
    (excode, sout,_) <- readProcessWithExitCode "stack" ["path","--project-root"] []
    case excode of
        ExitSuccess -> return $ init sout
        _ -> error "Could not determine stack project-root path"

-- | Execute an IO action if and only if it the current environment is inside a
-- docker container.
ensureDocker :: IO a -> IO a
ensureDocker cmd = do
    testResult <- isInsideDocker
    if testResult
        then cmd
        else error "This process must be run inside a docker container."

-- | Returns true if and only if the current envirionment is inside a docker
-- container.   Each docker container creates a file /.dockerenv, and the
-- existence of this file can be used to determine if a process is being run
-- inside a container.
isInsideDocker :: IO Bool
isInsideDocker = do
    (ex,_,serr)<-readProcessWithExitCode "find" ["/.dockerenv"] ""
    case ex of
        ExitSuccess -> return True
        ExitFailure 1 -> return False
        ExitFailure _ -> fail serr

-- | Copy src directory to archive folder
copyOutputToArchiveFolder :: FilePath -> IO ()
copyOutputToArchiveFolder src = do
  copyDirectory src "test-output"
  return ()

-- | Copy a directory.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
  let cmd  = "cp"
  let args = ["-rf", src, dst]
  let msg  = "\t" ++ cmd ++ " " ++ intercalate " " args
  safeRemoveDirectoryRecursive dst
  (excode, _sout, _serr) <- readProcessWithExitCode cmd args []
  case (==ExitSuccess) excode of
    True -> do
        putStrLn $ msg ++ " succeeded."
        return ()
    False -> do
        putStrLn $ msg ++ " failed: " ++ (show excode)
        return ()

-- Ensure Git is CLEAN
isGitClean :: IO Bool
isGitClean = do
    (ex,_,_)<-readProcessWithExitCode "git" ["diff","--quiet","HEAD"] ""
    return $ case ex of
        ExitSuccess -> True
        ExitFailure _ -> False

whenGitClean :: IO a -> IO a
whenGitClean action = do
    b<-isGitClean
    if b
        then action
        else fail "The git repository has uncommitted changes."
