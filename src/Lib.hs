module Lib where

import Protolude
import System.Process (readProcess)
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types

-- | Run @git diff ARGS@ in the current working directory.
gitDiff :: [Text] -> IO Text
gitDiff args = toS <$> readProcess "git" ("diff" : (toS <$> args)) ""

hunk :: Hunk -> IO ()
hunk Hunk {..} = print hunkLines

delta :: FileDelta -> IO ()
delta FileDelta {..} = case fileDeltaContent of
  Hunks hs -> forM_ hs hunk

exe :: IO ()
exe = do
  gd <- gitDiff ["origin/master"]
  let d = parseDiff gd
  case d of
    Right ds -> forM_ ds delta
    Left e -> putStrLn e
