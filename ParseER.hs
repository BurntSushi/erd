{-# LANGUAGE OverloadedStrings #-}
module ParseER
  ( loadER
  )
where

import Control.Monad (when, void)
import Data.Char (isSpace)
import Data.Maybe
import Data.Text.Lazy
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

toER :: [AST] -> Either String ER
toER = toER' Nothing
  where toER' :: Maybe Entity -> [AST] -> Either String ER
        toER' _ [] = Right $ ER [] []
        toER' Nothing (A a:_) =
          let name = show (field a)
           in Left $ printf "Attribute '%s' comes before first entity." name
        toER' (Just e) (A a:xs) = do
          er <- toER' (Just e) xs
          let (h:t) = entities er
          let h' = h { attrs = a:attrs h }
          return $ er { entities = h':t }
        toER' _ (E e:xs) = do
          er <- toER' (Just e) xs
          return $ er { entities = e:entities er }
        toER' e (R r:xs) = do
          er <- toER' e xs
          return $ er { rels = r:rels er }

document :: Parser [AST]
document = fmap catMaybes $ manyTill top eof
  where top = (try entity <?> "entity declaration")
              <|> (try attr <?> "attribute")
              <|> (try rel <?> "relationship")
              <|> (try comment <?> "comment")
              <|> blanks
        blanks = many1 (space <?> "whitespace") >> return Nothing

entity :: Parser (Maybe AST)
entity = do char '['
            n <- ident
            char ']'
            eolComment
            return $ Just $ E Entity { name = n, attrs = [] }

attr :: Parser (Maybe AST)
attr = do
  keys <- many (oneOf "*+" <|> do { spacesNoNew1; return ' ' })
  let (ispk, isfk) = ('*' `elem` keys, '+' `elem` keys)
  spacesNoNew
  n <- ident
  eolComment
  return $ Just $ A Attribute { field = n, pk = ispk, fk = isfk, elabel = "" }

rel :: Parser (Maybe AST)
rel = parserZero

comment :: Parser (Maybe AST)
comment = do
  char '#'
  manyTill anyChar $ try eol
  return Nothing

ident :: Parser Text
ident = fmap pack $ many1 alphaNum

eolComment :: Parser ()
eolComment = try spacesEol <|> try (void comment)

spacesNoNew :: Parser ()
spacesNoNew = skipMany $ satisfy $ \c -> c /= '\n' && c /= '\r' && isSpace c

spacesNoNew1 :: Parser ()
spacesNoNew1 = skipMany1 $ satisfy $ \c -> c /= '\n' && c /= '\r' && isSpace c

spacesEol :: Parser ()
spacesEol = spacesNoNew >> eol

eol :: Parser ()
eol = eof <|> do
  c <- oneOf "\n\r"
  when (c == '\r') $ optional $ char '\n'

