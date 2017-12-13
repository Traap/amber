{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module TestRunner.PrepareReportData
  ( formatTestEvidence
  , formatHistoryFiles
  , trimTrailingWhitespace
  , trimToLength
  , combineResultsFiles
  , appendResultsFile
  , stampPerformanceResults
  , prependCsvWithDirName
  , formatPerformanceFiles
  ) where

import Control.Monad (filterM)
import qualified Data.Text as T
import qualified Data.List as L
import System.Directory
import System.FilePath.Posix

-- | Trim trailing whitespace given an input string
trimTrailingWhitespace :: String -> String
trimTrailingWhitespace line = T.unpack . T.stripEnd . T.pack $ line

-- | Trim a line to a given length
trimToLength :: String -> Int -> String
trimToLength line len =
  if (L.length line < len)
     then line
     else (L.take len line) ++ " ..."

-- | Format a test output file for use as test evidence
-- in a test report document
formatTestEvidence :: FilePath -> IO ()
formatTestEvidence inPath = do
  inputFileExists <- doesFileExist inPath
  if inputFileExists
     then do
       inFile <- readFile inPath
       let inLines = lines inFile
       let trimmedLines = map (\l -> trimTrailingWhitespace l) inLines
       let outLines = map (\l -> trimLine l) trimmedLines
       writeFile (outPath inPath) (unlines outLines)
     else putStrLn (inPath ++ " file does not exist")

  where
  isHmstReq s = L.isInfixOf "hmstReq" s
  isHttpResp s = L.isInfixOf "httpResp" s
  trimLine s = if ((isHmstReq s) || (isHttpResp s))
                  then trimToLength s 110
                  else s
  outputFileName inputPath =
    (takeBaseName inputPath) ++ "-trimmed" ++ (takeExtension inputPath)
  outPath inputPath = (takeDirectory inputPath) </> (outputFileName inputPath)

-- | Identify the history.tex files from the test results directory
-- and format them for use as test evidence in a test report document
formatHistoryFiles :: FilePath -> IO ()
formatHistoryFiles resultsDir = do
  resultsDirExists <- doesDirectoryExist resultsDir
  if resultsDirExists
     then do
       putStrLn ("Formatting history files from " ++ resultsDir)
       dirList <- getDirectoryContents resultsDir
       let potentialFileList = map (\d -> historyFilePath d) dirList
       _ <- mapM (\h -> formatTestEvidence h) potentialFileList
       putStrLn "History files formatted"
     else
       putStrLn "No history files found"
  where
  historyFilePath dir = resultsDir </> dir </> "history.tex"

-- | Append the contents of a results.csv file to an output file
-- to be used to build the verification test report
appendResultsFile :: FilePath -> FilePath -> IO ()
appendResultsFile inPath outPath = do
  inputFileExists <- doesFileExist inPath
  if inputFileExists
     then do
       inFile <- readFile inPath
       let inLines = lines inFile
       appendFile outPath (unlines inLines)
     else putStrLn (inPath ++ " file does not exist")

-- | Identify the results.csv files from the test results directory
-- and combine them for use as test the master results list in building
-- a test report document
combineResultsFiles :: FilePath -> IO ()
combineResultsFiles resultsDir = do
  resultsDirExists <- doesDirectoryExist resultsDir
  if resultsDirExists
     then do
       putStrLn ("Combining results.csv files from " ++ resultsDir)
       dirList <- getDirectoryContents resultsDir
       let potentialFileList = map (\d -> resultsCsvFilePath d) dirList
       _ <- mapM (\r -> appendResultsFile r combinedCsvFile) potentialFileList
       putStrLn "Results CSV files combined"
     else
       putStrLn "No results.csv files found"
  where
  resultsCsvFilePath dir = resultsDir </> dir </> "results.csv"
  combinedCsvFile = resultsDir </> "combined-results.csv"

-- | Format a test results csv file for use as test evidence
-- in a test report document
stampPerformanceResults :: FilePath -> String -> String -> IO ()
stampPerformanceResults inPath runId protId = do
  inputFileExists <- doesFileExist inPath
  if inputFileExists
     then do
       inFile <- readFile inPath
       let inLines = filter (\s -> (isNotHeaderLine s)) $ lines inFile
       let newLines = map (\l -> prependStrToLine protId l) inLines
       let outLines = map (\l -> prependStrToLine runId l) newLines
       writeFile (outPath inPath) (unlines outLines)
     else putStrLn (inPath ++ " file does not exist")

  where
  prependStrToLine s l = s ++ "|" ++ l
  outputFileName inputPath =
    (takeBaseName inputPath) ++ "-formatted" ++ (takeExtension inputPath)
  outPath inputPath = (takeDirectory inputPath) </> (outputFileName inputPath)
  isHeaderLine s = L.isInfixOf "PatientId" s
  isNotHeaderLine s = not $ isHeaderLine s

-- | Prepend the results.csv files from the test results directory
-- with the directory name
prependCsvWithDirName :: FilePath -> IO ()
prependCsvWithDirName resultsDir = do
  resultsDirExists <- doesDirectoryExist resultsDir
  if resultsDirExists
     then do
       putStrLn ("Prepending CSV file with directory name for " ++ csvFilePath)
       _ <- stampPerformanceResults csvFilePath dirBaseName protId
       putStrLn "Results file updated"
     else
       putStrLn ("Results directory " ++ resultsDir ++ " not found")
  where
  csvFilePath = resultsDir </> "results.csv"
  dirBaseName = takeBaseName resultsDir
  protId = filter (/= '.') (takeExtension $ takeDirectory resultsDir)

-- | Identify the history.tex files from the test results directory
-- and format them for use as test evidence in a test report document
formatPerformanceFiles :: FilePath -> IO ()
formatPerformanceFiles resultsDir = do
  resultsDirExists <- doesDirectoryExist resultsDir
  if resultsDirExists
     then do
       putStrLn ("Formatting performance files from " ++ resultsDir)
       pathList <- listSubdirs resultsDir
       potentialSubdirList <- mapM (\d -> listSubdirs d ) pathList
       let finalSubdirList = concat potentialSubdirList
       _ <- mapM (\d -> prependCsvWithDirName d) finalSubdirList
       putStrLn "Performance files formatted"

       putStrLn ("Combining performance results files from " ++ resultsDir)

       let resultsFileList = map (\d -> d </> "results-formatted.csv") finalSubdirList
       -- clear any existing contents
       _ <- writeFile combinedCsvFile ""

       _ <- mapM (\r -> appendResultsFile r combinedCsvFile) resultsFileList
       putStrLn "Performance results files combined"
     else
       putStrLn "No performance results files found"

  where
  combinedCsvFile = resultsDir </> "combined-results.csv"

listSubdirs :: FilePath -> IO [FilePath]
listSubdirs d = do
  dirExists <- doesDirectoryExist d
  if dirExists
    then do
      putStrLn ("Finding list of subdirectory paths for " ++ d)
      contents <- getDirectoryContents d
      let subdirs = filter (\f -> head f /= '.') contents
      dirs <- filterM (\f -> (doesDirectoryExist (d </> f))) subdirs
      return $ map (\s -> d </> s) dirs
    else do
      putStrLn "Directory does not exist"
      return []
