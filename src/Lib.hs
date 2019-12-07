module Lib where

import Control.Monad.Combinators
import Control.Monad.Fail
import qualified Data.Text as T
import Hash
import Out
import Protolude hiding (check, many, some, sourceLine, try)
import System.Process (readProcess)
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types
import Text.Megaparsec hiding (region)
import Text.Megaparsec.Char
import Types

type Parser = Parsec Void Text

checksP :: Parser [Check]
checksP =
  catMaybes
    <$> many (Just <$> checkP <|> Nothing <$ nonCheckLine)
  where
    nonCheckLine = do
      _ <- takeWhileP (Just "non-check line char") nonNewline
      (newline >> pure () <|> eof)

nonNewline :: Char -> Bool
nonNewline = (/= '\n')

nonNewlineP = anySingleBut '\n'

-- Parses a newline-terminated line of text.
lineP :: Parser Text
lineP = takeWhileP (Just "non-newline char") nonNewline <* newline

-- | Parses a check.
checkP :: Parser Check
checkP = do
  -- If it can't pass a prefix then it backtracks. But if it can, then it
  -- commits to parsing a CHECK.
  prfx <- try (prefix <?> "prefix")
  space
  short <- takeWhileP (Just "short description") nonNewline <?> "title"
  newline
  long <- longP prfx <?> "description"
  stamp <- optional (stampP prfx <?> "STAMP")
  region <- regionP <?> "region"
  pure Check {..}

-- | Parses a prefix before a check.
prefix :: Parser Text
prefix = toS <$> manyTill (nonNewlineP <?> "comment prefix") (chunk "CHECK:")

-- | The long description is all lines that start with the prefix and are before
-- an optional stamp.
longP :: Text -> Parser [Text]
longP prfx = many (nonStampPrefix *> lineP)
  where
    nonStampPrefix = try (chunk prfx <* notFollowedBy (chunk "STAMP:"))

stampP :: Text -> Parser Stamp
stampP prfx = do
  chunk (prfx <> "STAMP: ")
  username <- toS <$> manyTill (nonNewlineP <?> "username char") (chunk " CHECKED ")
  (short, hash) <- first toS <$> manyTill_ (nonNewlineP <?> "short description") (hashP <?> "hash")
  newline
  pure Stamp {..}
  where
    hashP = between (chunk " (") (char ')') (takeP (Just "hash") 8)

regionP :: Parser Region
regionP = do
  start <- getSourcePos
  ls <- many nonEmptyLineP
  end <- getSourcePos
  pure Region
    { range = (unPos (sourceLine start), unPos (sourceLine end) - 1),
      content = ls,
      hash = sha256_8 (T.concat ls)
    }
  where
    nonEmptyLineP = T.cons <$> nonNewlineP <*> lineP

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

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f (x : xs) | Just y <- f x = (y : ys, xs')
  where
    (ys, xs') = spanMaybe f xs
spanMaybe _ xs = ([], xs)

clashes :: [Check] -> [Hunk] -> [(Check, [((Int, Int), Hunk)])]
clashes [] _ = []
clashes _ [] = []
clashes (c : cs) hs = if not (null is) then (c, is) : next else next
  where
    next = clashes cs hs
    (is, rest) = spanMaybe intersect (dropWhile before hs)
    before h = snd (hunkRange h) < fst (checkRange c)
    intersect h = (,h) <$> intervalIntersect (hunkRange h) (checkRange c)

checkRange :: Check -> (Int, Int)
checkRange Check {..} = range region

hunkRange :: Hunk -> (Int, Int)
hunkRange Hunk {hunkDestRange = Range {..}} =
  (rangeStartingLineNumber, rangeStartingLineNumber + rangeNumberOfLines -1)

intervalIntersect :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
intervalIntersect (a, b) (a', b') =
  if x <= y
    then pure (x, y)
    else Nothing
  where
    x = max a a'
    y = min b b'

delta :: FileDelta -> IO ()
delta FileDelta {..} = case fileDeltaContent of
  Hunks hs -> do
    let fp = toS fileDeltaDestFile
    t <- readFile fp
    case parse checksP fp t of
      Left err -> putStr (errorBundlePretty err)
      Right cs -> do
        let xs = clashes cs hs
        outAnsi
          ( Reminders
              { source = fp,
                reminders = (uncurry Reminder) <$> xs
              }
          )

exe :: IO ()
exe = do
  gd <- gitDiff ["origin/master", "--unified=0", "--minimal"]
  let d = parseDiff gd
  case d of
    Right ds -> forM_ ds delta
    Left e -> putStrLn e
