{-# LANGUAGE OverloadedStrings #-}

module Main
  (main)
where

import           Control.Applicative                 ((<|>))
import           Control.Monad                       (forM_, guard)
import qualified Data.ByteString                     as SB
import           Data.Maybe                          (fromJust, fromMaybe)
import qualified Data.Text.Lazy                      as L
import           System.Exit                         (exitFailure)
import           System.IO                           (hClose, hPutStrLn, stderr)

import           Data.GraphViz
import qualified Data.GraphViz.Attributes.Colors.X11 as C
import qualified Data.GraphViz.Attributes.Complete   as A
import qualified Data.GraphViz.Attributes.HTML       as H
import qualified Data.GraphViz.Types.Generalised     as G
import           Data.GraphViz.Types.Monadic

import           Erd.Config
import           Erd.ER
import           Erd.Parse
import           Erd.Render                          (htmlAttr, htmlFont,
                                                      recordAttr, withLabelFmt)

main :: IO ()
main = do
  checkRequirements -- application may terminate here
  conf <- configIO
  er' <- uncurry loadER (cin conf)
  case er' of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right er -> let erDot = dotER conf er
                    toFile h = SB.hGetContents h >>= SB.hPut (snd $ cout conf)
                    fmt = fromMaybe Pdf (outfmt conf)
                 in graphvizWithHandle Dot erDot fmt toFile
  hClose (snd $ cin conf)
  hClose (snd $ cout conf)

-- | Converts an entire ER-diagram from an ER file into a GraphViz graph.
dotER :: Config -> ER -> G.DotGraph L.Text
dotER conf er = graph' $ do
  graphAttrs (graphTitle $ title er)
  graphAttrs [ A.RankDir A.FromLeft
             , A.Splines $ fromConfigOrDefault edgeType
             ]
  nodeAttrs nodeGlobalAttributes
  edgeAttrs [ A.Color [A.toWC $ A.toColor C.Gray50] -- easier to read labels
            , A.MinLen 2 -- give some breathing room
            , A.Style [A.SItem (fromConfigOrDefault edgePattern) [] ]
            ]
  forM_ (entities er) $ \e ->
    node (name e) [entityFmt e]
  forM_ (rels er) $ relToEdge (fromConfigOrDefault notation)
    where
      fromConfigOrDefault :: (Config -> Maybe a) -> a
      fromConfigOrDefault opt = fromJust $ opt conf <|> opt defaultConfig
      nodeGlobalAttributes
        | fromConfigOrDefault dotentity = [shape Record, A.RankDir A.FromTop]
        | otherwise = [shape PlainText] -- recommended for HTML labels
      entityFmt
        | fromConfigOrDefault dotentity = toLabel . dotEntity
        | otherwise = toLabel . htmlEntity
      relToEdge n r = edge (entity1 r) (entity2 r) (label:eAttr n)
        where
          optss = roptions r
          labelOrXLabel =
            case fromConfigOrDefault edgeType of
              A.Ortho -> A.XLabel -- Graphivz recommends xlabels for ortho edges
              _ -> A.Label
          label =
            labelOrXLabel . A.HtmlLabel . H.Text $ withLabelFmt " %s " optss []
          (c1,c2) = (card1 r, card2 r)
          eAttr UML = [A.TailLabel $ card2label c1
                      ,A.HeadLabel $ card2label c2
                      ]
            where
              card2label = A.HtmlLabel . H.Text . htmlFont optss . L.pack . show
          eAttr IE = [A.Dir Both
                     ,A.ArrowTail $ card2arr c1
                     ,A.ArrowHead $ card2arr c2
                     ]
            where
              card2arr ZeroOne  = A.AType [(A.openMod, A.DotArrow), (A.noMods, A.Tee)]
              card2arr One      = A.AType [(A.noMods, A.Tee), (A.noMods, A.Tee)]
              card2arr ZeroPlus = A.AType [(A.noMods, A.Crow), (A.openMod, A.DotArrow)]
              card2arr OnePlus  = A.AType [(A.noMods, A.Crow), (A.noMods, A.Tee)]

-- | Converts a single entity to an HTML label.
htmlEntity :: Entity -> H.Label
htmlEntity e = H.Table H.HTable
                 { H.tableFontAttrs = Just $ optionsTo optToFont $ eoptions e
                 , H.tableAttrs = optionsTo optToHtml (eoptions e)
                 , H.tableRows = rows
                 }
  where rows = headerRow : map htmlAttr (attribs e)
        headerRow = H.Cells [H.LabelCell [] $ H.Text text]
        text = withLabelFmt " [%s]" (hoptions e) $ boldFont hname
        hname = htmlFont (hoptions e) (name e)
        boldFont s = [H.Format H.Bold s]

-- | Converts a single entity to a plain Dot Label
dotEntity :: Entity -> A.RecordFields
dotEntity e =  A.FieldLabel ( name e ) : map recordAttr (attribs e)

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
