{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_, when)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.String
import qualified Data.Text.Lazy as L
import System.Exit (exitFailure)
import System.IO (hClose, hPutStrLn, stderr)
import Text.Printf (printf)

import Data.GraphViz
import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Attributes.HTML as H
import Data.GraphViz.Commands.IO (putDot)
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic

import Config
import ER
import Parse

main :: IO ()
main = do
  conf <- configIO
  er' <- uncurry loadER (cin conf)
  case er' of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right er -> let dotted = dotER er
                 in putDot dotted >> runGraphvizCanvas Dot dotted Xlib
  hClose (snd $ cin conf)
  hClose (snd $ cout conf)

dotER :: ER -> G.DotGraph L.Text
dotER er = graph' $ do
  when (isJust $ title er) $ graphAttrs $ graphTitle $ fromJust $ title er
  nodeAttrs [shape PlainText]
  forM_ (entities er) $ \e ->
    node (name e) [toLabel (htmlEntity e)]
  forM_ (rels er) $ \r -> do
    let opts = roptions r
    let relLabel = A.HtmlLabel . H.Text . htmlFont opts . L.pack . show . rtype
    let (r1, r2) = (rel1 r, rel2 r) -- r1 is tail and r2 is head
    let (l1, l2) = (A.TailLabel $ relLabel r1, A.HeadLabel $ relLabel r2)
    let label = A.Label $ A.HtmlLabel $ H.Text $ withLabelFmt " %s " opts []
    edge (rname r1) (rname r2) [label, l1, l2]

htmlEntity :: Entity -> H.Label
htmlEntity e = H.Table H.HTable
                 { H.tableFontAttrs = Just $ fontAttrs (eoptions e)
                 , H.tableAttrs = tableAttrs (eoptions e)
                 , H.tableRows = rows
                 }
  where rows = headerRow : map htmlAttr (attribs e)
        headerRow = H.Cells [H.LabelCell [] $ H.Text text]
        text = withLabel headerOpts $ bold $ htmlText $ name e
        headerOpts = eoptions e `M.union` defaultHeaderAttrs
        bold s = [H.Format H.Bold s]

htmlAttr :: ER.Attribute -> H.Row
htmlAttr a = H.Cells [cell]
  where cell = H.LabelCell (cellAttrs opts) (H.Text $ withLabel opts name)
        name = fkfmt $ pkfmt $ htmlFont opts (field a)
        pkfmt s = if pk a then [H.Format H.Underline s] else s
        fkfmt s = if fk a then [H.Format H.Italics s] else s
        opts = aoptions a

tableAttrs :: Options -> [H.Attribute]
tableAttrs opts = mapMaybe optionToHtmlAttr
                  $ M.elems $ opts `M.union` defaultTableAttrs

cellAttrs :: Options -> [H.Attribute]
cellAttrs opts = H.Align H.HLeft
                 : mapMaybe optionToHtmlAttr (M.elems opts)

fontAttrs :: Options -> [H.Attribute]
fontAttrs = mapMaybe optionToFontAttr . M.elems

withLabel :: Options -> H.Text -> H.Text
withLabel = withLabelFmt " [%s]"

withLabelFmt :: String -> Options -> H.Text -> H.Text
withLabelFmt fmt opts s =
  case mapMaybe optionToLabel (M.elems opts) of
    (x:_) -> s ++ htmlFont opts (L.pack $ printf fmt $ L.unpack x)
    otherwise -> s

htmlFont :: Options -> L.Text -> H.Text
htmlFont opts s = [H.Font (fontAttrs opts) (htmlText s)]

htmlText :: L.Text -> H.Text
htmlText s = [H.Str s]

graphTitle :: L.Text -> [A.Attribute]
graphTitle s = if L.null s then [] else
  [ textLabel s
  , A.FontSize 30
  , A.LabelJust A.JLeft
  , A.LabelLoc A.VTop
  ]

defaultTableAttrs :: Options
defaultTableAttrs = M.fromList
  [ ("border", Border 0)
  , ("cellborder", CellBorder 1)
  , ("cellspacing", CellSpacing 0)
  , ("cellpadding", CellPadding 4)
  ]

defaultHeaderAttrs :: Options
defaultHeaderAttrs = M.fromList
  [ ("size", ER.FontSize 16)
  ]

