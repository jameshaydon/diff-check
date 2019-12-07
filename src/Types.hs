module Types where

import Protolude
import Text.Diff.Parse.Types

data Region
  = Region
      { range :: (Int, Int),
        content :: [Text],
        hash :: Text
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
        stamp :: Maybe Stamp,
        region :: Region
      }
  deriving (Show)

data Reminder
  = Reminder
      { check :: Check,
        hunks :: [((Int, Int), Hunk)]
      }

data Reminders
  = Reminders
      { source :: FilePath,
        reminders :: [Reminder]
      }
