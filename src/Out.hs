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

instance Out Stamp where
  out Stamp {..} =
    annotate italicized $
      "STAMP" <> ":" <+> pretty username <+> "CHECKED" <+> pretty short <+> parens (pretty hash)

instance Out Check where
  out Check {..} =
    vsep
      [ annotate (color Cyan <> underlined) "CHECK" <> ":" <+> pretty short,
        mempty,
        indent 2 $ annotate italicized (vsep (pretty <$> long))
      ]

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
    vsep
      [ out check,
        mempty,
        "The region of this check is affected by the following hunks:",
        mempty,
        vsep (out . snd <$> hunks),
        "To mark this as checked, use the stamp:",
        out (newStamp check)
      ]

instance Out Reminders where
  out Reminders {..} = case reminders of
    [] -> mempty
    _ ->
      vsep
        [ annotate bold (pretty source) <> ":" <+> pretty (length reminders) <+> "reminders:",
          mempty,
          vsep (out <$> reminders)
        ]

outAnsi :: Out a => a -> IO ()
outAnsi x = do
  hasColours <- Term.hSupportsANSI stdout
  let docStream = layoutSmart defaultLayoutOptions $ out x
      rdr = if hasColours then Term.renderStrict else Text.renderStrict
  putStr (rdr docStream)
