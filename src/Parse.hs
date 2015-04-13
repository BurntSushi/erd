{-# LANGUAGE OverloadedStrings #-}
module Parse
  ( loadER
  )
where

import Prelude hiding (null)

import Control.Monad (liftM2, when, void)
import Data.Char (isAlphaNum, isSpace)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe
import Data.Text.Lazy hiding (find, map, reverse)
import Data.Text.Lazy.IO
import System.IO (Handle)
import Text.Parsec
import Text.Parsec.Text.Lazy
import Text.Printf (printf)

import ER

data AST = E Entity
         | A Attribute
         | R Relation
         deriving Show

data GlobalOptions = GlobalOptions { gtoptions :: Options
                                   , ghoptions :: Options
                                   , geoptions :: Options
                                   , groptions :: Options
                                   }
                     deriving Show

emptyGlobalOptions :: GlobalOptions
emptyGlobalOptions = GlobalOptions M.empty M.empty M.empty M.empty

loadER :: String -> Handle -> IO (Either String ER)
loadER fpath f = do
  s <- hGetContents f
  case parse (do { (opts, ast) <- document; return $ toER opts ast}) fpath s of
    Left err -> return $ Left $ show err
    Right err@(Left _) -> return err
    Right (Right er) -> return $ Right er

-- | Converts a list of syntactic categories in an entity-relationship
-- description to an ER representation. If there was a problem with the
-- conversion, an error is reported. This includes checking that each
-- relationship contains only valid entity names.
--
-- This preserves the ordering of the syntactic elements in the original
-- description.
toER :: GlobalOptions -> [AST] -> Either String ER
toER gopts = toER' (ER [] [] title)
  where title = gtoptions gopts `mergeOpts` defaultTitleOpts

        toER' :: ER -> [AST] -> Either String ER
        toER' er [] = Right (reversed er) >>= validRels
        toER' (ER { entities = [] }) (A a:_) =
          let name = show (field a)
           in Left $ printf "Attribute '%s' comes before first entity." name
        toER' er@(ER { entities = e':es }) (A a:xs) = do
          let e = e' { attribs = a:attribs e' }
          toER' (er { entities = e:es }) xs
        toER' er@(ER { entities = es }) (E e:xs) = do
          let opts = eoptions e
                     `mergeOpts` geoptions gopts
                     `mergeOpts` defaultEntityOpts
          let hopts = eoptions e
                      `mergeOpts` ghoptions gopts
                      `mergeOpts` defaultHeaderOpts
          toER' (er { entities = e { eoptions = opts, hoptions = hopts }:es}) xs
        toER' er@(ER { rels = rs }) (R r:xs) = do
          let opts = roptions r
                     `mergeOpts` groptions gopts
                     `mergeOpts` defaultRelOpts
          toER' (er { rels = r { roptions = opts }:rs }) xs

        reversed :: ER -> ER
        reversed er@(ER { entities = es, rels = rs }) =
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

document :: Parser (GlobalOptions, [AST])
document = do skipMany (comment <|> blanks)
              opts <- globalOptions emptyGlobalOptions
              ast <- fmap catMaybes $ manyTill top eof
              return (opts, ast)
  where top = (entity <?> "entity declaration")
              <|> (try rel <?> "relationship") -- must come before attr
              <|> (try attr <?> "attribute")
              <|> (comment <?> "comment")
              <|> blanks
        blanks = many1 (space <?> "whitespace") >> return Nothing

entity :: Parser (Maybe AST)
entity = do n <- between (char '[') (char ']') ident
            spacesNoNew
            opts <- options
            eolComment
            return $ Just $ E Entity { name = n, attribs = [],
                                       hoptions = opts, eoptions = opts }

attr :: Parser (Maybe AST)
attr = do
  keys <- many $ oneOf "*+ \t"
  let (ispk, isfk) = ('*' `elem` keys, '+' `elem` keys)
  n <- ident
  opts <- options
  eolComment
  return
    $ Just
    $ A Attribute { field = n, pk = ispk, fk = isfk, aoptions = opts }

rel :: Parser (Maybe AST)
rel = do
  let ops = "?1*+"
  e1 <- ident
  op1 <- oneOf ops
  string "--"
  op2 <- oneOf ops
  e2 <- ident
  opts <- options

  let getCard op =
        case cardByName op of
          Just t -> return t
          Nothing -> unexpected (printf "Cardinality '%s' does not exist." op)
  t1 <- getCard op1
  t2 <- getCard op2
  return $ Just $ R Relation { entity1 = e1, entity2 = e2
                               , card1 = t1, card2 = t2, roptions = opts }

globalOptions :: GlobalOptions -> Parser GlobalOptions
globalOptions gopts =
  option gopts $ try $ do
    n <- ident 
    opts <- options
    case n of
      "title"        -> emptiness >> globalOptions (gopts { gtoptions = opts})
      "header"       -> emptiness >> globalOptions (gopts { ghoptions = opts})
      "entity"       -> emptiness >> globalOptions (gopts { geoptions = opts})
      "relationship" -> emptiness >> globalOptions (gopts { groptions = opts})
      _ -> fail "not a valid directive"

options :: Parser (M.Map String Option)
options =
  option M.empty
    $ fmap M.fromList
    $ try
    $ between (char '{' >> emptiness) (emptiness >> char '}')
    $ opt `sepEndBy` (emptiness >> char ',' >> emptiness)

opt :: Parser (String, Option)
opt = do
  name <- liftM2 (:) letter (manyTill (letter <|> char '-') (char ':'))
          <?> "option name"
  emptiness
  value <- between (char '"') (char '"') (many $ noneOf "\"")
           <?> "option value"
  case optionByName name value of
    Left err -> fail err
    Right o' -> emptiness >> return (name, o')

comment :: Parser (Maybe AST)
comment = do
  char '#'
  manyTill anyChar $ try eol
  return Nothing

ident :: Parser Text
ident = do
  spacesNoNew
  n <- identQuoted <|> identNoSpace
  spacesNoNew
  return n

identQuoted :: Parser Text
identQuoted = do
  char '`'
  let p = satisfy (\c -> c == '_' || c == ' ' || isAlphaNum c)
            <?> "letter, digit, space or underscore"
  n <- fmap pack (many1 p)
  char '`'
  return n

identNoSpace :: Parser Text
identNoSpace = do
  let p = satisfy (\c -> c == '_' || isAlphaNum c)
            <?> "letter, digit or underscore"
  n <- fmap pack (many1 p)
  return n

emptiness :: Parser ()
emptiness = skipMany (void (many1 space) <|> eolComment)

eolComment :: Parser ()
eolComment = spacesNoNew >> (eol <|> void comment)

spacesNoNew :: Parser ()
spacesNoNew = skipMany $ satisfy $ \c -> c /= '\n' && c /= '\r' && isSpace c

eol :: Parser ()
eol = eof <|> do
  c <- oneOf "\n\r"
  when (c == '\r') $ optional $ char '\n'

