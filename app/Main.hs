module Main where

import Config
import Lib
import Options.Applicative
import Protolude

main :: IO ()
main = do
  conf <- execParser opts
  exe conf
  where
    opts =
      info
        (config <**> helper)
        ( fullDesc
            <> progDesc "Print list of things to check based on what's changed against BRANCH"
            <> header "diffcheck - reminders triggered by git diffs"
        )
