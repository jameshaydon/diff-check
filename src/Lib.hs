module Lib (exe) where

import Config
import Control.Monad.Combinators
import qualified Data.Text as T
import DiffParse
import DiffTypes
import Hash
import Out
import Parse.Common
import Protolude hiding (check, hash, many, some, sourceLine, try)
import System.Directory
import qualified System.IO as IO
import System.Process (readProcess)
import Text.Megaparsec hiding (region)
import Text.Megaparsec.Char
import Types
import Prelude (getChar)

nonEolP :: Parser Char
nonEolP = anySingleBut '\n'

checksP :: Text -> Text -> Text -> Parser [Check]
checksP checkMark stampMark un = do
  xs <- many (Just <$> checkP checkMark stampMark un <|> Nothing <$ nonCheckLine)
  _ <- lastLine
  pure (catMaybes xs)
  where
    nonCheckLine = try $ takeWhileP (Just "non-check line char") nonEol >> eol
    lastLine = takeWhileP (Just "last line char") nonEol >> eof

-- | Parses a check.
checkP :: Text -> Text -> Text -> Parser Check
checkP checkMark stampMark username = do
  -- If it can't pass a prefix then it backtracks. But if it can, then it
  -- commits to parsing a CHECK.
  prefix <- try (prefixP checkMark <?> "prefix")
  space
  short <- takeWhileP (Just "short description") nonEol <?> "title"
  _ <- newline
  long <- longP stampMark prefix <?> "description"
  stampStart <- getOffset
  oldStamp <- optional (stampP stampMark prefix <?> "STAMP")
  stampEnd <- getOffset
  region <- regionP <?> "region"
  pure Check {newStamp = Stamp {hash = regionHash region, ..}, ..}

-- | Parses a prefix before a check.
prefixP :: Text -> Parser Text
prefixP checkMark = toS <$> manyTill (nonEolP <?> "comment prefix") (chunk checkMark)

-- | The long description is all lines that start with the prefix and are before
-- an optional stamp.
longP :: Text -> Text -> Parser [Text]
longP stampMark prfx = many (nonStampPrefix *> lineP)
  where
    nonStampPrefix = try (chunk prfx <* notFollowedBy (chunk stampMark))

stampP :: Text -> Text -> Parser Stamp
stampP stampMark prfx = do
  _ <- chunk (prfx <> stampMark)
  space
  username <- toS <$> manyTill (nonEolP <?> "username char") (chunk " CHECKED ")
  (short, hash) <- first toS <$> manyTill_ (nonEolP <?> "short description") (hashP <?> "hash")
  _ <- newline
  pure Stamp {..}
  where
    hashP = between (chunk " (") (char ')') (takeP (Just "hash") 8)

regionP :: Parser Region
regionP = do
  start <- getSourcePos
  ls <- many nonEmptyLineP
  end <- getSourcePos
  pure
    Region
      { range = (unPos (sourceLine start), unPos (sourceLine end) - 1),
        content = ls,
        regionHash = hashContent ls
      }
  where
    nonEmptyLineP = T.cons <$> nonEolP <*> lineP

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

delta :: Text -> Text -> Text -> FileDelta -> IO (Either Text [Reminder])
delta checkMark stampMark name FileDelta {..} = case fileDeltaStatus of
  Deleted -> pure (Right [])
  _ -> case fileDeltaContent of
    Hunks hs -> do
      let fp = toS fileDeltaDestFile
      modTime <- getModificationTime fp
      t <- readFile fp
      case parse (checksP checkMark stampMark name) fp t of
        Left err -> pure . Left . toS . errorBundlePretty $ err
        Right cs -> do
          let xs = clashes cs hs
          pure . Right $ uncurry (Reminder fp modTime) <$> xs
    _ -> pure (Right [])

say :: Text -> IO ()
say = putStrLn

formatStamp :: Text -> Text -> Stamp -> Text
formatStamp stampMark prfx Stamp {..} =
  prfx <> stampMark <> " " <> username <> " CHECKED " <> short <> " (" <> hash <> ")"

iMode :: Text -> Reminder -> IO ()
iMode stampMark r@Reminder {..} = do
  disp r
  say "\nMark as checked? This will add the stamp above. [y/n]"
  loop
  where
    loop = do
      IO.hSetBuffering stdin IO.NoBuffering
      cmd <- getChar
      IO.hSetBuffering stdin IO.LineBuffering
      case cmd of
        'y' -> do
          let Check {..} = check
          t <- readFile source
          let (beforeStamp, oldPlusRest) = T.splitAt stampStart t
              rest = T.drop (stampEnd - stampStart) oldPlusRest
              t' = beforeStamp <> formatStamp stampMark prefix newStamp <> "\n" <> rest
          modTime' <- getModificationTime source
          if modTime == modTime'
            then do
              writeFile source t'
              say "\nUpdated stamp."
            else say "\nFile was modified, aborting."
        'n' -> pure ()
        _ -> say "\nPlease type 'y' (yes) of 'n' (no):" >> loop

-- CAREFUL: just for testing
-- This is a line:
--   - This is an item;
--   - This is another.
-- CHECKPOINT: James Henri Haydon CHECKED just for testing (00pxTFJG)
exe :: Config -> IO ()
exe Config {..} = do
  say "diffcheck: looking for unstamped checks..."
  name <- gitUsername
  gd <- gitDiff [diffAgainst, "--unified=0", "--minimal"]
  if T.null gd
    then pure ()
    else do
      let d = parse diffP "diff" gd
      case d of
        Right ds -> do
          rs_ <- sequence <$> traverse (delta checkMarker stampMarker name) ds
          case rs_ of
            Left err -> putStrLn err >> exitFailure
            Right (concat -> []) -> do
              say "All clear."
              exitSuccess
            Right (concat -> rs) ->
              if interactive
                then traverse_ (iMode stampMarker) rs >> say "\nAll done."
                else disp rs >> exitFailure
        Left e -> do
          say "There was an error parsing the diff:"
          say (toS (errorBundlePretty e))
          exitFailure
