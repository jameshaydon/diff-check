module Config where

import Data.Semigroup ((<>))
import Options.Applicative
import Protolude hiding (option)

data Config
  = Config
      { diffAgainst :: Text,
        checkMarker :: Text,
        stampMarker :: Text,
        interactive :: Bool
      }

config :: Parser Config
config =
  Config
    <$> strOption
      ( long "diff-against"
          <> metavar "BRANCH"
          <> help "Which branch to diff against"
          <> showDefault
          <> value "origin/master"
      )
    <*> strOption
      ( long "check-marker"
          <> help "Marker to use for detecting CHECKs"
          <> showDefault
          <> value "CHECK:"
      )
    <*> strOption
      ( long "stamp-marker"
          <> help "Marker to use for detecting STAMPs"
          <> showDefault
          <> value "STAMP:"
      )
    <*> switch
      ( long "interactive"
          <> short 'i'
          <> help "Use the interactive mode"
      )
