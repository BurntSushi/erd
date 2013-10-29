{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import Data.String
import qualified Data.Text.Lazy as L
import System.Exit (exitFailure)
import System.IO (hClose, hPutStrLn, stderr)

import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
-- import Data.GraphViz.Commands 
import Data.GraphViz.Commands.IO
import Data.GraphViz.Printing
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic

import Config
import ER
import Parse

data PortNode = NText L.Text
               | NPort L.Text L.Text
               deriving (Eq, Ord)

instance IsString PortNode where
  fromString = NText . fromString

instance PrintDot PortNode where
  unqtDot (NText t) = unqtDot t
  unqtDot (NPort table field) = unqtDot table <> text ":" <> unqtDot field

  toDot (NText t) = toDot t
  toDot (NPort table field) = toDot table <> text ":" <> toDot field

eg :: G.DotGraph PortNode
eg = digraph' $ do
  graphAttrs [RankDir FromLeft]
  nodeAttrs [shape Record]
  node "test" [textLabel "<l1> left | <r1> right"]
  node "test2" [textLabel "<l2> left2 | {<a> A | <b> B}"]

  NPort "test" "l1" --> NPort "test2" "b"

dotER :: ER -> G.DotGraph PortNode
dotER er = digraph' $ do
  graphAttrs [RankDir FromLeft]
  nodeAttrs [shape Record]
  forM_ (entities er) $ \e ->
    node (NText $ name e) []

main :: IO ()
main = do
  conf <- configIO
  er' <- uncurry loadER (cin conf)
  case er' of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right er -> putDot $ dotER er
  hClose (snd $ cin conf)
  hClose (snd $ cout conf)
  -- runGraphvizCanvas' eg Xlib 

