module TestRunner.PrettyPrint
  ( ppDoc
  , ppLine
  , ppLines
  , ppSout
  , ppString
  ) where

import System.FilePath
import System.IO
import Text.PrettyPrint.HughesPJ

-- | Maximum style line length.
styleLineLength :: Int
styleLineLength = 80

-- | Pretty Print style in use.
ppStyle :: Style
ppStyle = Style
        {mode=PageMode
        ,lineLength=styleLineLength
        ,ribbonsPerLine=0.1
        }

-- | Pretty print line.
ppLine :: String -> [Doc]
ppLine s =
    let isLongLine        = length s > styleLineLength
        hasPathSeparators = any (== pathSeparator) s
        isLongPath        = isLongLine && hasPathSeparators
        _words            = if isLongPath
                            then splitPath
                            else words
    in map text $ _words s

-- | Pretty print lines.
ppLines :: String -> [[Doc]]
ppLines s = map ppLine $ lines s

-- | Pretty print doc.
ppDoc :: String -> [Doc]
ppDoc s = map fsep $ ppLines s

-- | Prepare a string for pretty printing.
ppString :: String -> [String]
ppString s = map (renderStyle ppStyle) $ ppDoc s

-- | Print strings to provided handle.
ppSout :: Handle -> String -> IO ()
ppSout h s = do
  let ppStr = ppString s
  mapM_ (hPutStrLn h) ppStr
