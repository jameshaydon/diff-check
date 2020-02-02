module Parse.Common where

import Protolude
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

isEol :: Char -> Bool
isEol = (== '\n')

nonEol :: Char -> Bool
nonEol = not . isEol

-- | Takes text up to but not including the end of line.
lineP :: Parser Text
lineP = takeWhileP (Just "non-newline char") nonEol <* eol
