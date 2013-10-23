module ER
  ( ER(..)
  , Entity(..)
  , Attribute(..)
  , Relation(..)
  , Rel(..)
  , RelType(..)
  )
where

import Data.Text.Lazy

data ER = ER { entities :: [Entity], rels :: [Relation] }
          deriving Show

data Entity = Entity { name :: Text, attrs :: [Attribute] }
              deriving Show

instance Eq Entity where
  e1 == e2 = (toLower . name) e1 == (toLower . name) e2

instance Ord Entity where
  e1 `compare` e2 = (toLower . name) e1 `compare` (toLower . name) e2

data Attribute = Attribute { field :: Text
                           , pk :: Bool
                           , fk :: Bool
                           , elabel :: Text
                           }
                 deriving Show

instance Eq Attribute where
  a1 == a2 = (toLower . field) a1 == (toLower . field) a2

instance Ord Attribute where
  a1 `compare` a2 = (toLower . field) a1 `compare` (toLower . field) a2

data Relation = Relation { rel1 :: Rel
                         , rel2 :: Rel
                         , rlabel :: Text
                         }
                deriving Show

data Rel = Rel { rname :: Text, rtype :: RelType }
           deriving Show

data RelType = None
             | ZeroOne
             | One
             | ZeroPlus
             | OnePlus
             deriving Show

