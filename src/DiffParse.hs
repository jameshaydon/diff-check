module DiffParse where

import Control.Monad.Combinators
import qualified Data.Char as Char
import DiffTypes
import Parse.Common
import Protolude hiding (check, diff, hash, many, option, some, sourceLine, try)
import Text.Megaparsec hiding (region)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

diffP :: Parser FileDeltas
diffP = some fileDelta <* eof

fileDelta :: Parser FileDelta
fileDelta = do
  (status, source, dest) <- fileDeltaHeader
  content <- try binary <|> hunks
  pure (FileDelta status source dest content)
  where
    binary =
      Binary
        <$ string "Binary files "
        <* path
        <* string " and "
        <* path
        <* string " differ"
        <* eol
    hunks = Hunks <$> many hunk
      where
        hunk =
          Hunk <$> ("@@ -" *> range)
            <*> (" +" *> range <* " @@" <* lineP)
            <*> many annotatedLine
        range = Range <$> Lex.decimal <*> option 1 ("," *> Lex.decimal)

fileDeltaHeader :: Parser (FileStatus, Text, Text)
fileDeltaHeader = do
  _ <- string "diff --git "
  source <- path <* space
  dest <- path <* eol
  status <- fileStatus
  ignoreLine "index"
  ignoreLine "--- "
  ignoreLine "+++ "
  pure (status, source, dest)
  where
    ignoreLine w = optional (string w >> lineP) >> pure ()

fileStatus :: Parser FileStatus
fileStatus = extendedHeaders >> (created <|> deleted <|> pure Modified)
  where
    header x = string x >> lineP
    created = Created <$ header "new file mode"
    deleted = Deleted <$ header "deleted file mode"
    exts =
      header
        <$> [ "old mode",
              "new mode",
              "copy from",
              "copy to",
              "rename from",
              "rename to",
              "similarity index",
              "dissimilarity index",
              "index"
            ]
    extendedHeaders = many (choice exts)

path :: Parser Text
path = optional (letterChar >> string "/") *> takeWhileP (Just "path") (not . (liftA2 (||) Char.isSpace isEol))

annotatedLine :: Parser Line
annotatedLine =
  Line
    <$> annotation
    <*> (lineP <* optional (string "\\ No newline at end of file" <* eol))
  where
    annotation =
      (Added <$ char '+')
        <|> (Removed <$ char '-')
        <|> (Context <$ char ' ')
