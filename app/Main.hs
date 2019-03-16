{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, guard)
import qualified Data.ByteString as SB
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as L
import System.Exit (exitFailure)
import System.IO (hClose, hPutStrLn, stderr)
import Text.Printf (printf)

import Data.GraphViz
import qualified Data.GraphViz.Attributes.Colors.X11 as C
import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Attributes.HTML as H
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Commands (isGraphvizInstalled)

import qualified Erd.ER as ER

import Erd.Config
import Erd.ER
import Erd.Parse

main :: IO ()
main = do
  checkRequirements -- application may terminate here
  conf <- configIO
  er' <- uncurry loadER (cin conf)
  case er' of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right er -> let dotted = dotER er
                    toFile h = SB.hGetContents h >>= SB.hPut (snd $ cout conf)
                    fmt = fromMaybe Pdf (outfmt conf)
                 in graphvizWithHandle Dot dotted fmt toFile
  hClose (snd $ cin conf)
  hClose (snd $ cout conf)

-- | Converts an entire ER-diagram from an ER file into a GraphViz graph.
dotER :: ER -> G.DotGraph L.Text
dotER er = graph' $ do
  graphAttrs (graphTitle $ title er)
  graphAttrs [A.RankDir A.FromLeft]
  nodeAttrs [shape PlainText] -- recommended for HTML labels
  edgeAttrs [ A.Color [A.toWC $ A.toColor C.Gray50] -- easier to read labels
            , A.MinLen 2 -- give some breathing room
            , A.Style [A.SItem A.Dashed []] -- easier to read labels, maybe?
            ]
  forM_ (entities er) $ \e ->
    node (name e) [toLabel (htmlEntity e)]
  forM_ (rels er) $ \r -> do
    let opts = roptions r
    let rlab = A.HtmlLabel . H.Text . htmlFont opts . L.pack . show
    let (l1, l2) = (A.TailLabel $ rlab $ card1 r, A.HeadLabel $ rlab $ card2 r)
    let label = A.Label $ A.HtmlLabel $ H.Text $ withLabelFmt " %s " opts []
    edge (entity1 r) (entity2 r) [label, l1, l2]

-- | Converts a single entity to an HTML label.
htmlEntity :: Entity -> H.Label
htmlEntity e = H.Table H.HTable
                 { H.tableFontAttrs = Just $ optionsTo optToFont $ eoptions e
                 , H.tableAttrs = optionsTo optToHtml (eoptions e)
                 , H.tableRows = rows
                 }
  where rows = headerRow : map htmlAttr (attribs e)
        headerRow = H.Cells [H.LabelCell [] $ H.Text text]
        text = withLabelFmt " [%s]" (hoptions e) $ bold hname
        hname = htmlFont (hoptions e) (name e)
        bold s = [H.Format H.Bold s]

-- | Converts a single attribute to an HTML table row.
htmlAttr :: ER.Attribute -> H.Row
htmlAttr a = H.Cells [cell]
  where cell = H.LabelCell cellAttrs (H.Text $ withLabelFmt " [%s]" opts name)
        name = fkfmt $ pkfmt $ htmlFont opts (field a)
        pkfmt s = if pk a then [H.Format H.Underline s] else s
        fkfmt s = if fk a then [H.Format H.Italics s] else s
        cellAttrs = H.Align H.HLeft : optionsTo optToHtml opts
        opts = aoptions a

-- | Formats HTML text with a label. The format string given should be
-- in `Data.Text.printf` style. (Only font options are used from the options
-- given.)
withLabelFmt :: String -> Options -> H.Text -> H.Text
withLabelFmt fmt opts s =
  case optionsTo optToLabel opts of
    (x:_) -> s ++ htmlFont opts (L.pack $ printf fmt $ L.unpack x)
    _ -> s

-- | Formats an arbitrary string with the options given (using only font
-- attributes).
htmlFont :: Options -> L.Text -> H.Text
htmlFont opts s = [H.Font (optionsTo optToFont opts) [H.Str s]]

-- | Extracts and formats a graph title from the options given.
-- The options should be title options from an ER value.
-- If a title does not exist, an empty list is returned and `graphAttrs attrs`
-- should be a no-op.
graphTitle :: Options -> [A.Attribute]
graphTitle topts =
  let glabel = optionsTo optToLabel topts
  in if null glabel then [] else
       [ A.LabelJust A.JLeft
       , A.LabelLoc A.VTop
       , A.Label $ A.HtmlLabel $ H.Text $ htmlFont topts (head glabel)
       ]

checkRequirements :: IO ()
checkRequirements = (isGraphvizInstalled >>= guard) <|> quitWithoutGraphviz msg
  where
    msg = "GraphViz is not installed on your system.\n" ++
          "Please install it first, https://github.com/BurntSushi/erd"
