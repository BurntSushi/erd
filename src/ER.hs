{-# LANGUAGE OverloadedStrings #-}
module ER
  ( ER(..)
  , Entity(..)
  , Attribute(..)
  , Option(..)
  , Relation(..)
  , Rel(..)
  , RelType(..)
  , optionByName
  , relTypeByName
  )
where

import Data.Text.Lazy
import Text.Printf (printf)

import Data.GraphViz.Parsing (Parse, parse, runParser)
import Data.GraphViz.Attributes.Colors (Color)

data ER = ER { entities :: [Entity], rels :: [Relation] }
          deriving Show

data Entity = Entity { name :: Text
                     , attrs :: [Attribute]
                     , eoptions :: [Option]
                     }
              deriving Show

instance Eq Entity where
  e1 == e2 = name e1 == name e2

instance Ord Entity where
  e1 `compare` e2 = name e1 `compare` name e2

data Attribute = Attribute { field :: Text
                           , pk :: Bool
                           , fk :: Bool
                           , aoptions :: [Option]
                           }
                 deriving Show

instance Eq Attribute where
  a1 == a2 = field a1 == field a2

instance Ord Attribute where
  a1 `compare` a2 = field a1 `compare` field a2

data Option = Label String
            | Color Color
            | BgColor String
            deriving Show

optionByName :: String -> String -> Either String Option
optionByName "label" = Right . Label
optionByName "color" = optionColor
optionByName "bgcolor" = optionColor
optionByName unk = const (Left $ printf "Option '%s' does not exist." unk)

optionColor :: String -> Either String Option
optionColor cstr =
  case fst $ runParser (parse :: Parse Color) quoted of
    Left err -> Left (printf "%s (bad color '%s')" err cstr)
    Right clr -> Right (Color clr)
  where quoted = "\"" `append` pack cstr `append` "\""

data Relation = Relation { rel1 :: Rel
                         , rel2 :: Rel
                         , roptions :: [Option]
                         }
                deriving Show

data Rel = Rel { rname :: Text, rtype :: RelType }
           deriving Show

data RelType = ZeroOne
             | One
             | ZeroPlus
             | OnePlus
             deriving Show

relTypeByName :: Char -> Maybe RelType
relTypeByName '?' = Just ZeroOne
relTypeByName '1' = Just One
relTypeByName '*' = Just ZeroPlus
relTypeByName '+' = Just OnePlus
relTypeByName _ = Nothing

