module Out where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal as Term
import Data.Text.Prettyprint.Doc.Render.Text as Text
import Protolude hiding (check, hash)
import qualified System.Console.ANSI as Term
import Text.Diff.Parse.Types
import Types

class Out a where
  out :: a -> Doc AnsiStyle

instance Out Check where
  out Check {..} =
    vsep $ [ch <+> tit] <> desc
    where
      ch = annotate (color Blue <> underlined) "CHECK" <> ":"
      tit = annotate bold (pretty short) <+> parens hashChange
      hashChange = oldHash <> " → " <> newHash
      newHash = annotate (color Green) (pretty (regionHash region))
      oldHash = case oldStamp of
        Nothing -> mempty
        Just Stamp {..} -> annotate (color Red) (pretty hash)
      desc = case long of
        [] -> []
        _ -> [indent 2 $ annotate italicized (vsep (pretty <$> long))]

instance Out Line where
  out Line {..} = case lineAnnotation of
    Added -> annotate (color Green) $ "+" <> pretty lineContent
    Removed -> annotate (color Red) $ "-" <> pretty lineContent
    Context -> " " <> pretty lineContent

instance Out Hunk where
  out Hunk {..} =
    indent 2 (vsep diffs) <> hardline
    where
      diffs = "L" <> pretty (rangeStartingLineNumber hunkDestRange) : (out <$> hunkLines)

instance Out Reminder where
  out Reminder {..} =
    hardline
      <> vsep
        [ pretty source,
          out check,
          mempty,
          "The region of this check is affected by the following hunks:",
          mempty,
          vsep (out <$> hunks)
        ]

instance Out [Reminder] where
  out = \case
    [] -> mempty
    rs -> vsep (out <$> rs)

outAnsi :: Out a => a -> IO ()
outAnsi x = do
  hasColours <- Term.hSupportsANSI stdout
  let docStream = layoutSmart defaultLayoutOptions $ out x
      rdr = if hasColours then Term.renderStrict else Text.renderStrict
  putStr (rdr docStream)
