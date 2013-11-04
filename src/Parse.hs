{-# LANGUAGE OverloadedStrings #-}
module Parse
  ( loadER
  )
where

import Control.Monad (liftM2, when, void)
import Data.Char (isSpace)
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
toER :: Options -> [AST] -> Either String ER
toER opts' = toER' (ER [] [] graphTitle)
  where graphTitle = optionToLabel $ M.findWithDefault (Label "") "label" opts'
        opts = M.delete "label" opts'

        toER' :: ER -> [AST] -> Either String ER
        toER' er [] = Right (reversed er) >>= validRels
        toER' (ER { entities = [] }) (A a:_) =
          let name = show (field a)
           in Left $ printf "Attribute '%s' comes before first entity." name
        toER' er@(ER { entities = e:es }) (A a:xs) = do
          let e' = e { attribs = a:attribs e }
          toER' (er { entities = e':es }) xs
        toER' er@(ER { entities = es }) (E e:xs) = do
          let e' = e { eoptions = eoptions e `M.union` opts }
          toER' (er { entities = e':es}) xs
        toER' er@(ER { rels = rs }) (R r:xs) =
          toER' (er { rels = r:rs }) xs

        reversed :: ER -> ER
        reversed er@(ER { entities = es, rels = rs }) =
          let es' = map (\e -> e { attribs = reverse (attribs e) }) es
          in  er { entities = reverse es', rels = reverse rs }

        validRels :: ER -> Either String ER
        validRels er = validRels' (rels er) er

        validRels' :: [Relation] -> ER -> Either String ER
        validRels' [] er = return er
        validRels' (r:rs) er = do
          let r1 = find (\e -> name e == rname (rel1 r)) (entities er)
          let r2 = find (\e -> name e == rname (rel2 r)) (entities er)
          let err getter = Left
                             $ printf "Unknown entity '%s' in relationship."
                             $ unpack $ rname $ getter r
          when (isNothing r1) (err rel1)
          when (isNothing r2) (err rel2)
          return er
          

document :: Parser (Options, [AST])
document = do skipMany (comment <|> blanks)
              opts <- options
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
            return $ Just $ E Entity { name = n, attribs = [], eoptions = opts }

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

  t1 <-
    case relTypeByName op1 of
      Just t1 -> return t1
      Nothing -> unexpected (printf "Relation type '%s' does not exist." op1)
  t2 <-
    case relTypeByName op2 of
      Just t2 -> return t2
      Nothing -> unexpected (printf "Relation type '%s' does not exist." op2)
  let r1 = Rel { rname = e1, rtype = t1 }
  let r2 = Rel { rname = e2, rtype = t2 }
  return $ Just $ R Relation { rel1 = r1, rel2 = r2, roptions = opts }

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
  n <- fmap pack (many1 alphaNum)
  spacesNoNew
  return n

emptiness :: Parser ()
emptiness = skipMany (void (many1 space) <|> eolComment)

eolComment :: Parser ()
eolComment = spacesNoNew >> (eol <|> void comment)

spacesNoNew :: Parser ()
spacesNoNew = skipMany $ satisfy $ \c -> c /= '\n' && c /= '\r' && isSpace c

spacesEol :: Parser ()
spacesEol = spacesNoNew >> eol

eol :: Parser ()
eol = eof <|> do
  c <- oneOf "\n\r"
  when (c == '\r') $ optional $ char '\n'

