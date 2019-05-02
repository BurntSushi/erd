{-# LANGUAGE OverloadedStrings #-}
module Main
  (main)
where

import           Control.Applicative                 ((<|>))
import           Control.Monad                       (forM_, guard)
import qualified Data.ByteString                     as SB
import           Data.Maybe                          (fromMaybe)
import qualified Data.Text.Lazy                      as L
import           System.Exit                         (exitFailure)
import           System.IO                           (hClose, hPutStrLn, stderr)

import           Data.GraphViz
import qualified Data.GraphViz.Attributes            as A
import qualified Data.GraphViz.Attributes.Colors.X11 as C
import qualified Data.GraphViz.Attributes.Complete   as A
import qualified Data.GraphViz.Attributes.HTML       as H
import           Data.GraphViz.Commands              (isGraphvizInstalled)
import qualified Data.GraphViz.Types.Generalised     as G
import           Data.GraphViz.Types.Monadic

import           Erd.Config
import           Erd.ER
import           Erd.Parse
import           Erd.Render                          (htmlAttr, htmlFont,
                                                      withLabelFmt)

main :: IO ()
main = do
  checkRequirements -- application may terminate here
  conf <- configIO
  er' <- uncurry loadER (cin conf)
  case er' of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right er -> let dotted = dotER conf er
                    toFile h = SB.hGetContents h >>= SB.hPut (snd $ cout conf)
                    fmt = fromMaybe Pdf (outfmt conf)
                 in graphvizWithHandle Dot dotted fmt toFile
  hClose (snd $ cin conf)
  hClose (snd $ cout conf)

-- | Converts an entire ER-diagram from an ER file into a GraphViz graph.
dotER :: Config -> ER -> G.DotGraph L.Text
dotER conf er = graph' $ do
  graphAttrs (graphTitle $ title er)
  graphAttrs [ A.RankDir A.FromLeft
             , A.Splines (edgeType conf)
             ]
  nodeAttrs [shape PlainText] -- recommended for HTML labels
  edgeAttrs [ A.Color [A.toWC $ A.toColor C.Gray50] -- easier to read labels
            , A.MinLen 2 -- give some breathing room
            , A.Style [A.SItem A.Dashed []] -- easier to read labels, maybe?
            ]
  forM_ (entities er) $ \e ->
    node (name e) [toLabel (htmlEntity e)]
  forM_ (rels er) renderEdge

-- | Renders the edges between nodes. When available edges terminate
-- appropriately to matching fields. In order to have edge-porting properly
-- working, field and table-name need be aligned.
renderEdge :: Relation -> Dot L.Text
renderEdge r = drawEdge labelOfEdge r
  where
    labelOfEdge :: (A.Attribute, A.Attribute)
    labelOfEdge = (A.HeadLabel . rlab r . card2 $ r,
                   A.TailLabel . rlab r . card1 $ r)

    rlab :: Show a => Relation -> a -> A.Label
    rlab x = A.HtmlLabel . H.Text . htmlFont (roptions x) . L.pack . show

drawEdge :: (A.Attribute, A.Attribute) -> Relation -> Dot L.Text
drawEdge l r = edge (entity1 r) (entity2 r) ([label, fst l, snd l] <> edgeOf r)
  where
    label = A.Label . A.HtmlLabel . H.Text $ withLabelFmt " %s " (roptions r) []

edgeOf :: Relation -> A.Attributes
edgeOf r
  | card1 r == ZeroOne || card1 r == One =
      [A.TailPort $ A.LabelledPort portId Nothing,
       A.HeadPort $ A.LabelledPort (portnameOn entity1) Nothing]
  | otherwise =
      [A.TailPort $ A.LabelledPort (portnameOn entity2) Nothing,
       A.HeadPort $ A.LabelledPort portId Nothing]
  where
    portnameOn fn = A.PN $ (L.toLower . L.replace " " "_" $ fn r) <> "_" <> termId
    portId        = A.PN termId
    termId        = "id"

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
