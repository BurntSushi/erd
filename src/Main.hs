{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.String
import qualified Data.Text.Lazy as L
import System.Exit (exitFailure)
import System.IO (hClose, hPutStrLn, stderr)

import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
-- import Data.GraphViz.Commands 
-- import Data.GraphViz.Commands.IO 
import Data.GraphViz.Printing
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Monadic

import Config
import Parse

data ERLabel = ERText L.Text
               | ERRecord L.Text L.Text
               deriving (Eq, Ord)

instance IsString ERLabel where
  fromString = ERText . fromString

instance PrintDot ERLabel where
  unqtDot (ERText t) = unqtDot t
  unqtDot (ERRecord table field) = unqtDot table <> text ":" <> unqtDot field

  toDot (ERText t) = toDot t
  toDot (ERRecord table field) = toDot table <> text ":" <> toDot field

eg :: G.DotGraph ERLabel
eg = digraph' $ do
  graphAttrs [RankDir FromLeft]
  nodeAttrs [shape Record]
  node "test" [textLabel "<l1> left | <r1> right"]
  node "test2" [textLabel "<l2> left2 | {<a> A | <b> B}"]

  ERRecord "test" "l1" --> ERRecord "test2" "b"

main :: IO ()
main = do
  conf <- configIO
  er' <- loadER "wat" (cin conf)
  case er' of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right er -> print er
  hClose (cin conf)
  hClose (cout conf)
  -- putDot eg 
  -- runGraphvizCanvas' eg Xlib 

