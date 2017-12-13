module TestRunner.Protocol (Protocol(..)) where

-- Type: Protocol

data Protocol = PROTOCOL
  {pn :: IO ()            -- Protocol name.
  ,msg :: String          -- Message text.
  }

