module TestRunner.Latex
   ( beginTabular
   , beginVerbatim
   , centering
   , endTabular
   , endVerbatim
   , inputFile
   , tableHeader
   , toPDF
   )  where

import System.Exit
import System.Process
import TestRunner.Program

-------------------------------------------------------------------------------
-- Constants needed to write text that LeTex can use to properly render a PDF.
-------------------------------------------------------------------------------
beginVerbatim :: String
beginVerbatim = "\\lstset{language=[]}\\begin{lstlisting}"

endVerbatim :: String
endVerbatim   = "\\end{lstlisting}"

beginTabular :: String
beginTabular  = "\\begin{longtable}{|C{1.2cm}|L{1.2cm}|L{10cm}|C{2cm}|}\\hline"

tableHeader :: String
tableHeader   = "Step & IUR & Confirm / Expectation & Result\\\\ \\hline"

centering :: String
centering     = "\\centering"

endTabular :: String
endTabular    = "\\end{longtable}"

inputFile :: String -> String
inputFile fname = "\\input{" ++ fname ++ "}"

-------------------------------------------------------------------------------
-- These functions convert LaTex files to a pdf report.
-------------------------------------------------------------------------------
report :: String -> String -> [Program]
report fpath fname =
  [PROGRAM
    {p="pdflatex"
    ,a=["--no-shell-escape"
       ,"--interaction=nonstopmode"
       ,"--output-directory"
       ,fpath
       ,fname
       ]
    }
  ,PROGRAM
    {p="pdflatex"
    ,a=["--no-shell-escape"
       ,"--interaction=nonstopmode"
       ,"--output-directory"
       ,fpath
       ,fname
       ]
    }
  ]

-- | Assemble a PDF document using TeX source files.  The first pass assembles
-- the document and the second phase generates a table of contents.
toPDF :: String -> String -> IO ()
toPDF fpath fname = do
  _ <- mapM assemblePDF (report fpath fname)
  return ()

-- | Run a command and return its exit code.
assemblePDF :: Program -> IO ExitCode
assemblePDF program = do
  (excode, _sout, _serr) <- readProcessWithExitCode (p program) (a program) []
  return excode

