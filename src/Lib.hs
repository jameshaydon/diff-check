module Lib where

import qualified Data.Text as T
import Hash
import Protolude
import System.Process (readProcess)
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types

getReminder :: Text -> Maybe (Maybe Text)
getReminder line =
  let (before, and) = T.breakOn "REMIND" line
   in undefined

say :: Text -> IO ()
say = putStrLn

-- | Run @git diff ARGS@ in the current working directory.
gitDiff :: [Text] -> IO Text
gitDiff args = toS <$> readProcess "git" ("diff" : (toS <$> args)) ""

line :: Line -> IO ()
line Line {..} = do
  putStrLn $ T.cons ann lineContent
  where
    ann = case lineAnnotation of
      Added -> '+'
      Removed -> '-'

hunk :: Hunk -> IO ()
hunk Hunk {..} = do
  print hunkSourceRange
  print hunkDestRange
  forM_ hunkLines line

delta :: FileDelta -> IO ()
delta FileDelta {..} = case fileDeltaContent of
  Hunks hs -> do
    say $ "source: " <> fileDeltaSourceFile
    say $ "dest: " <> fileDeltaDestFile
    say ""
    forM_ hs hunk

exe :: IO ()
exe = do
  gd <- gitDiff ["origin/master", "--unified=0", "--minimal", "--", "app/Main.hs"]
  let d = parseDiff gd
  case d of
    Right ds -> forM_ ds delta
    Left e -> putStrLn e
