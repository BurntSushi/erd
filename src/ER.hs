{-# LANGUAGE OverloadedStrings #-}
module ER
  ( ER(..)
  , Entity(..)
  , Attribute(..)
  , Options(..)
  , Option(..)
  , Relation(..)
  , Rel(..)
  , RelType(..)
  , optionByName
  , optionToFontAttr
  , optionToHtmlAttr
  , optionToLabel
  , relTypeByName
  )
where

import qualified Data.Map as M
import Data.Text.Lazy
import Data.Word (Word8)
import Text.Printf (printf)

import Data.GraphViz.Parsing (Parse, ParseDot, parse, runParser)
import qualified Data.GraphViz.Attributes.HTML as H
import Data.GraphViz.Attributes.Colors (Color)

data ER = ER { entities :: [Entity]
             , rels :: [Relation]
             , title :: Maybe Text
             }
          deriving Show

data Entity = Entity { name :: Text
                     , attribs :: [Attribute]
                     , eoptions :: Options
                     }
              deriving Show

instance Eq Entity where
  e1 == e2 = name e1 == name e2

instance Ord Entity where
  e1 `compare` e2 = name e1 `compare` name e2

data Attribute = Attribute { field :: Text
                           , pk :: Bool
                           , fk :: Bool
                           , aoptions :: Options
                           }
                 deriving Show

instance Eq Attribute where
  a1 == a2 = field a1 == field a2

instance Ord Attribute where
  a1 `compare` a2 = field a1 `compare` field a2

type Options = M.Map String Option

data Option = Label String
            | BgColor Color
            | Color Color
            | FontFace Text
            | FontSize Double
            | Border Word8
            | BorderColor Color
            | CellSpacing Word8
            | CellBorder Word8
            | CellPadding Word8
            deriving Show

optionByName :: String -> String -> Either String Option
optionByName "label" = Right . Label
optionByName "color" = optionParse Color
optionByName "bgcolor" = optionParse BgColor
optionByName "size" = optionParse FontSize
optionByName "font" = optionParse FontFace
optionByName "border" = optionParse Border
optionByName "border-color" = optionParse BorderColor
optionByName "cellspacing" = optionParse CellSpacing
optionByName "cellborder" = optionParse CellBorder
optionByName "cellpadding" = optionParse CellPadding
optionByName unk = const (Left $ printf "Option '%s' does not exist." unk)

optionParse :: ParseDot a => (a -> Option) -> String -> Either String Option
optionParse con s =
  case fst $ runParser parse quoted of
    Left err -> Left (printf "%s (bad value '%s')" err s)
    Right a -> Right (con a)
  where quoted = "\"" `append` pack s `append` "\""

optionToFontAttr :: Option -> Maybe H.Attribute
optionToFontAttr (Color c) = Just $ H.Color c
optionToFontAttr (FontFace s) = Just $ H.Face s
optionToFontAttr (FontSize d) = Just $ H.PointSize d
optionToFontAttr _ = Nothing

optionToHtmlAttr :: Option -> Maybe H.Attribute
optionToHtmlAttr (BgColor c) = Just $ H.BGColor c
optionToHtmlAttr (Border w) = Just $ H.Border w
optionToHtmlAttr (BorderColor c) = Just $ H.Color c
optionToHtmlAttr (CellSpacing w) = Just $ H.CellSpacing w
optionToHtmlAttr (CellBorder w) = Just $ H.CellBorder w
optionToHtmlAttr (CellPadding w) = Just $ H.CellPadding w
optionToHtmlAttr _ = Nothing

optionToLabel :: Option -> Maybe Text
optionToLabel (Label s) = Just $ pack s
optionToLabel _ = Nothing

data Relation = Relation { rel1 :: Rel
                         , rel2 :: Rel
                         , roptions :: Options
                         }
                deriving Show

data Rel = Rel { rname :: Text, rtype :: RelType }
           deriving Show

data RelType = ZeroOne
             | One
             | ZeroPlus
             | OnePlus

instance Show RelType where
  show ZeroOne = "{0,1}"
  show One = "1"
  show ZeroPlus = "0..N"
  show OnePlus ="1..N"

relTypeByName :: Char -> Maybe RelType
relTypeByName '?' = Just ZeroOne
relTypeByName '1' = Just One
relTypeByName '*' = Just ZeroPlus
relTypeByName '+' = Just OnePlus
relTypeByName _ = Nothing

