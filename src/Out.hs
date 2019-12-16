module Out where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal as Term
import Data.Text.Prettyprint.Doc.Render.Text as Text
import Data.Text.Prettyprint.Doc.Util
import Protolude hiding (check, hash)
import qualified System.Console.ANSI as Term
import qualified System.Console.Terminal.Size as Term
import Text.Diff.Parse.Types
import Types

class Out a where
  out :: a -> Doc AnsiStyle

instance Out Check where
  out Check {..} = vsep (top : desc)
    where
      top =
        sep
          [ annotate (color Blue <> underlined <> bold) "CHECK" <> ":",
            annotate bold (pretty short),
            parens (oldHash <> " ➜ " <> newHash)
          ]
      newHash = paint Green (pretty (regionHash region))
      oldHash = case oldStamp of
        Nothing -> mempty
        Just s -> paint Red (pretty (hash s))
      desc = case long of
        [] -> []
        _ -> [indent 2 $ annotate italicized (vsep (pretty <$> long))]

instance Out Line where
  out Line {..} = case lineAnnotation of
    Added -> paint Green ("+" <> c)
    Removed -> paint Red ("-" <> c)
    Context -> " " <> c
    where
      c = pretty lineContent

instance Out Hunk where
  out Hunk {..} = indent 2 (vsep diffs) <> hardline
    where
      diffs = "L" <> pretty (rangeStartingLineNumber hunkDestRange) : (out <$> hunkLines)

instance Out Reminder where
  out Reminder {..} =
    hardline
      <> vsep
        [ pretty source,
          out check,
          mempty,
          reflow "The region of this check is affected by the following hunks:",
          mempty,
          vsep (out <$> hunks)
        ]

instance Out [Reminder] where
  out = \case
    [] -> mempty
    rs -> vsep (out <$> rs)

disp :: Out a => a -> IO ()
disp x = do
  hasAnsi <- Term.hSupportsANSI stdout
  w_ <- Term.size
  let width = fromMaybe 80 (Term.width <$> w_)
      opts = LayoutOptions {layoutPageWidth = AvailablePerLine width 1.0}
      docStream = layoutSmart opts (out x)
      rdr = if hasAnsi then Term.renderStrict else Text.renderStrict
  putStr (rdr docStream)

paint :: Color -> Doc AnsiStyle -> Doc AnsiStyle
paint c = annotate (color c)
