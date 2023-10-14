{-# LANGUAGE OverloadedStrings #-}

module Text.Parsec.Erd.Parser
  ( AST(..),
    GlobalOptions(..),
    document,
    globalOptions,
    entity,
    rel,
    attr,
    comment
  ) where

import           Erd.ER

import           Control.Monad         (liftM2, void, when)
import           Data.Char             (isAlphaNum, isControl, isSpace)
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Text.Lazy        hiding (elem)
import           Text.Parsec
import           Text.Parsec.Text.Lazy
import           Text.Printf           (printf)

data AST = E Entity
    | A Attribute
    | R Relation
    deriving (Show, Eq)

data GlobalOptions = GlobalOptions
    { gtoptions :: Options
    , ghoptions :: Options
    , geoptions :: Options
    , groptions :: Options
    }
    deriving (Show, Eq)

emptyGlobalOptions :: GlobalOptions
emptyGlobalOptions = GlobalOptions M.empty M.empty M.empty M.empty

document :: Parser (GlobalOptions, [AST])
document = do skipMany (comment <|> blanks)
              opts <- globalOptions emptyGlobalOptions
              ast <-  catMaybes <$> manyTill top eof
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
    $ A Attribute {field = n, pk = ispk, fk = isfk, aoptions = opts <> defaultAttrOpts}

rel :: Parser (Maybe AST)
rel = do
  let ops = "?1*+"
  e1 <- ident
  op1 <- oneOf ops
  _ <- string "--"
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
  optName <- liftM2 (:) letter (manyTill (letter <|> char '-') (char ':'))
          <?> "option name"
  emptiness
  value <- between (char '"') (char '"') (many $ noneOf "\"")
           <?> "option value"
  case optionByName optName value of
    Left err -> fail err
    Right o' -> emptiness >> return (optName, o')

comment :: Parser (Maybe AST)
comment = do
  _ <- char '#'
  _ <- manyTill anyChar $ try eol
  return Nothing

ident :: Parser Text
ident = do
  spacesNoNew
  n <- identQuoted <|> identNoSpace
  spacesNoNew
  return n

identQuoted :: Parser Text
identQuoted = do
  quote <- oneOf "'\"`"
  let p = satisfy (\c -> c /= quote && not (isControl c) )
            <?> "any character except " ++ [quote] ++ " or control characters"
  n <- fmap pack (many1 p)
  _ <- char quote
  return n

identNoSpace :: Parser Text
identNoSpace = do
  let p = satisfy (\c -> c == '_' || isAlphaNum c)
            <?> "letter, digit or underscore"
  fmap pack (many1 p)

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

