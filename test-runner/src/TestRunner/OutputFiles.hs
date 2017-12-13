module TestRunner.OutputFiles
  ( openEquipmentFile
  , openRecipeFile
  , openRecipeResultsFile
  , openResultsFile
  , openStepResultsFile
  , recipeResultsFile
  , stepResultsFile
  , tmpPath
  ) where

import System.IO

-------------------------------------------------------------------------------
-- The follow functions assemble strings.
-------------------------------------------------------------------------------
equipment :: String
equipment = "equipment.tex"

recipe :: String
recipe = "recipe.tex"

recipeResults :: String
recipeResults = "recipe-results-"

results :: String
results = "results.tex"

stepNbr :: String
stepNbr = "step-"

tmpPath :: String -> String
tmpPath s = s ++ "/"

-------------------------------------------------------------------------------
-- The follow functions are used to open files written to that read when a LaTex
-- document is converted to a pdf document.
-------------------------------------------------------------------------------

-- | Open Recipe File.  Recipe steps are written to the recipe file in a LaTex
-- tabular format.
openRecipeFile :: FilePath -> IOMode -> IO Handle
openRecipeFile fpath fmode = do
  let f = tmpPath fpath ++ recipe
  h <- openFile' f fmode
  return h

-- | Open Recipe Results File.  Recipe results will be PASS or FAIL when written.
openRecipeResultsFile :: FilePath -> IOMode -> Int -> IO Handle
openRecipeResultsFile fpath fmode nbr = do
  let f = recipeResultsFile fpath nbr
  h <- openFile' f fmode
  return h

-- | Recipe Results file name:  recipe-results-n.tex
recipeResultsFile :: FilePath -> Int -> String
recipeResultsFile f i = tmpPath f
                      ++ recipeResults
                      ++ (show i)
                      ++ ".tex"

-- | Open Results File.  The results of running a recipe are written to this
-- file.
openResultsFile :: FilePath -> IOMode -> IO Handle
openResultsFile fpath fmode = do
  let f = tmpPath fpath ++ results
  h <- openFile' f fmode
  return h

-- | Open Step Results File.  Step results will be PASS or FAIL when written.
openStepResultsFile :: FilePath -> IOMode -> Int -> IO Handle
openStepResultsFile fpath fmode nbr = do
  let f = stepResultsFile fpath nbr
  h <- openFile' f fmode
  return h

-- | Step Results file name:  step-n.tex
stepResultsFile :: FilePath -> Int -> String
stepResultsFile f i = tmpPath f
                    ++ stepNbr
                    ++ (show i)
                    ++ ".tex"

-- | Open Equipment File. The operating system environment is written to this
-- file.
openEquipmentFile :: FilePath -> IOMode -> IO Handle
openEquipmentFile fpath fmode = do
  let f = tmpPath fpath ++ equipment
  h <- openFile' f fmode
  return h

-- | Open a file.
openFile' :: FilePath -> IOMode -> IO Handle
openFile' fpath fmode = do
  putStrLn $ "\t" ++ (show fmode) ++ ": " ++ fpath
  h <- openFile fpath fmode
  return h
