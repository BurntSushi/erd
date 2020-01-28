{-# LANGUAGE OverloadedStrings #-}

module Erd.Render
  (htmlAttr,
   htmlFont,
   recordAttr,
   withLabelFmt
  ) where

import qualified Erd.ER                            as ER

import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Attributes.HTML     as H
import qualified Data.Text.Lazy                    as L
import           Text.Printf                       (printf)

-- | Converts a single attribute to an HTML table row.
htmlAttr :: ER.Attribute -> H.Row
htmlAttr a = H.Cells [cell]
  where cell    = H.LabelCell cellAttrs (H.Text $ withLabelFmt " [%s]" opts name)
        name    = fkfmt $ pkfmt $ htmlFont opts (ER.field a)
        pkfmt s = if ER.pk a then [H.Format H.Underline s] else s
        fkfmt s = if ER.fk a then [H.Format H.Italics s] else s
        opts    = ER.aoptions a
        cellAttrs = ER.optionsTo ER.optToHtml opts
-- | Converts a single attribute to a RecordField ( an element of a dot table )
recordAttr :: ER.Attribute -> A.RecordField
recordAttr a = A.FieldLabel $ ER.field a -- should change to add port support!
-- | Formats an arbitrary string with the options given (using only font
-- attributes).
htmlFont :: ER.Options -> L.Text -> H.Text
htmlFont opts s = [H.Font (ER.optionsTo ER.optToFont opts) [H.Str s]]

-- | Formats HTML text with a label. The format string given should be
-- in `Data.Text.printf` style. (Only font options are used from the options
-- given.)
withLabelFmt :: String -> ER.Options -> H.Text -> H.Text
withLabelFmt fmt opts s =
  case ER.optionsTo ER.optToLabel opts of
    (x:_) -> s ++ htmlFont opts (L.pack $ printf fmt $ L.unpack x)
    _     -> s
