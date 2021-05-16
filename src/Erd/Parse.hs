{-# LANGUAGE OverloadedStrings #-}
module Erd.Parse
  ( loadER
  )
where

import           Erd.ER

import           Control.Exception      (bracket_)
import           Control.Monad          (when,unless)
import           Data.List              (find)
import           Data.Maybe
import           Data.Text.Lazy         hiding (find, map, reverse)
import           Data.Text.Lazy.IO
import           System.IO              (Handle,hIsTerminalDevice,hSetEncoding,
                                         hGetEncoding,utf8)
import           Text.Parsec
import           Text.Parsec.Erd.Parser (AST (..), GlobalOptions (..), document)
import           Text.Printf            (printf)

loadER :: String -> Handle -> IO (Either String ER)
loadER fpath f = do
  Just initialEncoding <- hGetEncoding f
  isTerminalDevice <- hIsTerminalDevice f
  let setEncodingIfNeeded = unless isTerminalDevice . hSetEncoding f
  bracket_
    (setEncodingIfNeeded utf8)
    (setEncodingIfNeeded initialEncoding)
    (do
      s <- hGetContents f
      case parse (do { (opts, ast) <- document; return $ toER opts ast}) fpath s of
        Left err           -> return $ Left $ show err
        Right err@(Left _) -> return err
        Right (Right er)   -> return $ Right er
    )

-- | Converts a list of syntactic categories in an entity-relationship
-- description to an ER representation. If there was a problem with the
-- conversion, an error is reported. This includes checking that each
-- relationship contains only valid entity names.
--
-- This preserves the ordering of the syntactic elements in the original
-- description.
toER :: GlobalOptions -> [AST] -> Either String ER
toER gopts = toER' (ER [] [] erTitle)
  where erTitle = gtoptions gopts `mergeOpts` defaultTitleOpts

        toER' :: ER -> [AST] -> Either String ER
        toER' er [] = Right (reversed er) >>= validRels
        toER' ER { entities = [] } (A a:_) =
          let fieldName = show (field a)
          in  Left $ printf "Attribute '%s' comes before first entity." fieldName
        toER' er@ER { entities = e':es } (A a:xs) = do
          let e = e' { attribs = a:attribs e' }
          toER' (er { entities = e:es }) xs
        toER' er@ER { entities = es } (E e:xs) = do
          let opts = eoptions e
                     `mergeOpts` geoptions gopts
                     `mergeOpts` defaultEntityOpts
          let hopts = eoptions e
                      `mergeOpts` ghoptions gopts
                      `mergeOpts` defaultHeaderOpts
          toER' (er { entities = e { eoptions = opts, hoptions = hopts }:es}) xs
        toER' er@ER { rels = rs } (R r:xs) = do
          let opts = roptions r
                     `mergeOpts` groptions gopts
                     `mergeOpts` defaultRelOpts
          toER' (er { rels = r { roptions = opts }:rs }) xs

        reversed :: ER -> ER
        reversed er@ER { entities = es, rels = rs } =
          let es' = map (\e -> e { attribs = reverse (attribs e) }) es
          in  er { entities = reverse es', rels = reverse rs }

        validRels :: ER -> Either String ER
        validRels er = validRels' (rels er) er

        validRels' :: [Relation] -> ER -> Either String ER
        validRels' [] er = return er
        validRels' (r:_) er = do
          let r1 = find (\e -> name e == entity1 r) (entities er)
          let r2 = find (\e -> name e == entity2 r) (entities er)
          let err getter = Left
                             $ printf "Unknown entity '%s' in relationship."
                             $ unpack $ getter r
          when (isNothing r1) (err entity1)
          when (isNothing r2) (err entity2)
          return er
