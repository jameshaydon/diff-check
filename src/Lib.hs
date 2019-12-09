module Lib (exe) where

import Control.Monad.Combinators
import qualified Data.Text as T
import Hash
import Out
import Protolude hiding (check, hash, many, some, sourceLine, try)
import System.Process (readProcess)
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types
import Text.Megaparsec hiding (region)
import Text.Megaparsec.Char
import Types

type Parser = Parsec Void Text

checksP :: Text -> Parser [Check]
checksP un =
  catMaybes
    <$> many (Just <$> checkP un <|> Nothing <$ nonCheckLine)
  where
    nonCheckLine = takeWhileP (Just "non-check line char") nonNewline >> newline >> pure () <|> eof

nonNewline :: Char -> Bool
nonNewline = (/= '\n')

nonNewlineP :: Parser Char
nonNewlineP = anySingleBut '\n'

-- Parses a newline-terminated line of text.
lineP :: Parser Text
lineP = takeWhileP (Just "non-newline char") nonNewline <* newline

-- | Parses a check.
checkP :: Text -> Parser Check
checkP username = do
  -- If it can't pass a prefix then it backtracks. But if it can, then it
  -- commits to parsing a CHECK.
  prfx <- try (prefix <?> "prefix")
  space
  short <- takeWhileP (Just "short description") nonNewline <?> "title"
  _ <- newline
  long <- longP prfx <?> "description"
  oldStamp <- optional (stampP prfx <?> "STAMP")
  region <- regionP <?> "region"
  pure Check {newStamp = Stamp {hash = regionHash region, ..}, ..}

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
  _ <- chunk (prfx <> "STAMP: ")
  username <- toS <$> manyTill (nonNewlineP <?> "username char") (chunk " CHECKED ")
  (short, hash) <- first toS <$> manyTill_ (nonNewlineP <?> "short description") (hashP <?> "hash")
  _ <- newline
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
      regionHash = hashContent ls
    }
  where
    nonEmptyLineP = T.cons <$> nonNewlineP <*> lineP

hashContent :: [Text] -> Text
hashContent = sha256_8 . T.intercalate "\n"

-- | Run @git diff ARGS@ in the current working directory.
gitDiff :: [Text] -> IO Text
gitDiff args = toS <$> readProcess "git" ("diff" : (toS <$> args)) ""

gitUsername :: IO Text
gitUsername = do
  un <- toS <$> readProcess "git" ["config", "user.name"] ""
  pure (T.strip un)

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f (x : xs) | Just y <- f x = (y : ys, xs')
  where
    (ys, xs') = spanMaybe f xs
spanMaybe _ xs = ([], xs)

clashes :: [Check] -> [Hunk] -> [(Check, [Hunk])]
clashes [] _ = []
clashes _ [] = []
clashes (c : cs) hs = if include then (c, is) : next else next
  where
    next = clashes cs rest
    include = not (validStamp c || null is)
    (is, rest) = spanMaybe intersect (dropWhile before hs)
    before h = snd (hunkRange h) < fst (checkRange c)
    intersect h = h <$ intervalIntersect (hunkRange h) (checkRange c)

-- (,h) <$> intervalIntersect (hunkRange h) (checkRange c)

validStamp :: Check -> Bool
validStamp Check {region = Region {regionHash = h}, oldStamp} = case oldStamp of
  Just Stamp {hash = h'} | h == h' -> True
  _ -> False

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

delta :: Text -> FileDelta -> IO (Either Text [Reminder])
delta name FileDelta {..} = case fileDeltaContent of
  Hunks hs -> do
    let fp = toS fileDeltaDestFile
    t <- readFile fp
    case parse (checksP name) fp t of
      Left err -> pure . Left . toS . errorBundlePretty $ err
      Right cs -> do
        let xs = clashes cs hs
        pure . Right $ (uncurry (Reminder fp)) <$> xs
  _ -> pure . Right $ []

exe :: IO ()
exe = do
  name <- gitUsername
  gd <- gitDiff ["origin/master", "--unified=0", "--minimal"]
  if T.null gd
    then pure ()
    else
      let d = parseDiff gd
       in case d of
            Right ds -> do
              rs_ <- sequence <$> traverse (delta name) ds
              case rs_ of
                Left err -> putStrLn err >> exitFailure
                Right (concat -> []) -> exitSuccess
                Right (concat -> rs) -> outAnsi rs >> exitFailure
            Left e -> putStrLn e
