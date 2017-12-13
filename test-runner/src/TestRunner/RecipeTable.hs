module TestRunner.RecipeTable
  ( putRecipeTable
  ) where

import System.IO
import TestRunner.Latex
import TestRunner.OutputFiles
import TestRunner.Step

-------------------------------------------------------------------------------
-- The following functions create a LaTex table containing human-readable text
-- describing the recipe: Step #, Req #, Confirmation and Expected results.
-------------------------------------------------------------------------------

-- | record the recipe
putRecipeTable :: [Step] -> String -> IOMode -> IO ()
putRecipeTable steps fpath fmode = do
  h <- openRecipeFile fpath fmode
  putRecipeTableHeader h
  mapM_ (putRecipeTableRow h fpath) steps
  putRecipeTableFooter h
  hClose h

-- | record the header row for a recipe.
putRecipeTableHeader :: Handle -> IO ()
putRecipeTableHeader h = do
  hPutStrLn h $ beginTabular
  hPutStrLn h $ tableHeader

-- | record a table row for a recipe.
putRecipeTableRow :: Handle -> String -> Step -> IO ()
putRecipeTableRow h fpath step = do
  let link = stepResultsFile fpath (n step)
  hPutStrLn h $ (show (n step)) ++ " & "
                ++ (r step)  ++ " & "
                ++ (c step)  ++ "\\newline "
                ++ (x step)  ++ " & "
                ++ inputFile link ++ "\\\\ \\hline"

-- | record the footer row for a recipe.
putRecipeTableFooter :: Handle -> IO ()
putRecipeTableFooter h = do
  hPutStrLn h $ endTabular

