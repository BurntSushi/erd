{-# LANGUAGE OverloadedStrings #-}
module Parse
  ( loadER
  )
where

import Control.Monad (liftM2, when, void)
import Data.Char (isSpace)
import Data.Maybe
import Data.Text.Lazy hiding (map, reverse)
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
  case parse (do { ast <- document; return $ toER ast}) fpath s of
    Left err -> return $ Left $ show err
    Right err@(Left _) -> return err
    Right (Right er) -> return $ Right er

-- | Converts a list of syntactic categories in an entity-relationship
-- description to an ER representation. If there was a problem with the
-- conversion, an error is reported.
--
-- This preserves the ordering of the syntactic elements in the original
-- description.
toER :: [AST] -> Either String ER
toER = toER' (ER [] [])
  where toER' :: ER -> [AST] -> Either String ER
        toER' er [] = Right $ reversed er
        toER' (ER { entities = [] }) (A a:_) =
          let name = show (field a)
           in Left $ printf "Attribute '%s' comes before first entity." name
        toER' er@(ER { entities = e:es }) (A a:xs) = do
          let e' = e { attrs = a:attrs e }
          toER' (er { entities = e':es }) xs
        toER' er@(ER { entities = es }) (E e:xs) =
          toER' (er { entities = e:es}) xs
        toER' er@(ER { rels = rs }) (R r:xs) =
          toER' (er { rels = r:rs }) xs

        reversed :: ER -> ER
        reversed (ER { entities = es, rels = rs }) =
          let es' = map (\e -> e { attrs = reverse (attrs e) }) es
          in  ER { entities = reverse es', rels = reverse rs }

document :: Parser [AST]
document = fmap catMaybes $ manyTill top eof
  where top = (try entity <?> "entity declaration")
              <|> (try attr <?> "attribute")
              <|> (try rel <?> "relationship")
              <|> (try comment <?> "comment")
              <|> blanks
        blanks = many1 (space <?> "whitespace") >> return Nothing

entity :: Parser (Maybe AST)
entity = do n <- between (char '[') (char ']') ident
            spacesNoNew
            opts <- options
            eolComment
            return $ Just $ E Entity { name = n, attrs = [], eoptions = opts }

attr :: Parser (Maybe AST)
attr = do
  keys <- many $ oneOf "*+ \t"
  let (ispk, isfk) = ('*' `elem` keys, '+' `elem` keys)
  spacesNoNew
  n <- ident
  opts <- options
  return
    $ Just
    $ A Attribute { field = strip n, pk = ispk, fk = isfk, aoptions = opts }

options :: Parser [Option]
options =
  option []
    $ try
    $ between (char '{' >> emptiness) (emptiness >> char '}')
    $ opt `sepEndBy` (emptiness >> char ',' >> emptiness)

opt :: Parser Option
opt = do
  name <- liftM2 (:) letter (manyTill letter (char ':'))
          <?> "option name"
  emptiness
  value <- between (char '"') (char '"') (many $ noneOf "\"")
           <?> "option value"
  case optionByName name value of
    Just o' -> emptiness >> return o'
    Nothing -> unexpected (printf "Option '%s' does not exist." name)

rel :: Parser (Maybe AST)
rel = parserZero

comment :: Parser (Maybe AST)
comment = do
  char '#'
  manyTill anyChar $ try eol
  return Nothing

ident :: Parser Text
ident = fmap pack (many1 (alphaNum <|> char ' ' <|> char '\t'))

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

