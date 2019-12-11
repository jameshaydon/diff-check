module Types where

import Data.Time
import Protolude
import Text.Diff.Parse.Types

data Region
  = Region
      { range :: (Int, Int),
        content :: [Text],
        regionHash :: Text
      }
  deriving (Show)

data Stamp
  = Stamp
      { username :: Text,
        hash :: Text,
        short :: Text
      }
  deriving (Show)

data Check
  = Check
      { short :: Text,
        long :: [Text],
        oldStamp :: Maybe Stamp,
        stampStart :: Int,
        stampEnd :: Int,
        newStamp :: Stamp,
        region :: Region,
        prefix :: Text
      }
  deriving (Show)

data Reminder
  = Reminder
      { source :: FilePath,
        modTime :: UTCTime,
        check :: Check,
        hunks :: [Hunk]
      }
