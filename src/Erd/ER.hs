{-# LANGUAGE OverloadedStrings #-}
module Erd.ER
  ( ER(..)
  , Entity(..)
  , Attribute(..)
  , Options, mergeOpts, optionsTo
  , Option(..), optionByName, optToFont, optToHtml, optToLabel
  , Relation(..) , Cardinality(..), cardByName
  , defaultAttrOpts, defaultTitleOpts, defaultEntityOpts, defaultHeaderOpts, defaultRelOpts
  )
where

import qualified Data.Map                        as M
import           Data.Maybe                      (mapMaybe)
import           Data.Text.Lazy
import           Data.Word                       (Word8)
import           Text.Printf                     (printf)

import           Data.GraphViz.Attributes.Colors (Color)
import qualified Data.GraphViz.Attributes.HTML   as H
import           Data.GraphViz.Parsing           (ParseDot, parse, runParser)

-- | Represents a single schema.
data ER = ER
    { entities :: [Entity]
    , rels     :: [Relation]
    , title    :: Options
    }
    deriving (Show, Eq)

-- | Represents a single entity in a schema.
data Entity = Entity
    { name     :: Text
    , attribs  :: [Attribute]
    , hoptions :: Options
    , eoptions :: Options
    }
    deriving (Show, Eq)

instance Ord Entity where
  e1 `compare` e2 = name e1 `compare` name e2

-- | Represents a single attribute in a particular entity.
data Attribute = Attribute
    { field    :: Text
    , pk       :: Bool
    , fk       :: Bool
    , aoptions :: Options
    }
    deriving (Show, Eq)

instance Ord Attribute where
  a1 `compare` a2 = field a1 `compare` field a2

-- | Represents any number of options for an item in an ER diagram.
-- An item may be the graph title, an entity, an entity header or a
-- relationship between entities. Keys are options as specified in ER files.
--
-- Note that a set of options may include a label for any item.
type Options = M.Map String Option

-- | Given two sets of options, merge the second into first, where elements
-- in the first take precedence.
mergeOpts :: Options -> Options -> Options
mergeOpts opts1 opts2 = opts1 `M.union` opts2

-- | Given a set of options and a selector function, return the list of
-- only those options which matched. Examples of the selector function are
-- `optToFont`, `optToHtml` and `optToLabel`.
optionsTo :: (Option -> Maybe a) -> Options -> [a]
optionsTo f = mapMaybe f . M.elems

-- | A restricted subset of options in GraphViz that can be configured in
-- an ER file.
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
    | TextAlignment H.Align
    deriving (Show, Eq)

-- | Given an option name and a string representation of its value,
-- `optionByName` will attempt to parse the string as a value corresponding
-- to the option. If the option doesn't exist or there was a problem parsing
-- the value, an error is returned.
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
optionByName "text-alignment" = optionParse TextAlignment
optionByName unk = const (Left $ printf "Option '%s' does not exist." unk)

-- | A wrapper around the GraphViz's parser for any particular option.
optionParse :: ParseDot a => (a -> Option) -> String -> Either String Option
optionParse con s =
  case fst $ runParser parse quoted of
    Left err -> Left (printf "%s (bad value '%s')" err s)
    Right a  -> Right (con a)
  where quoted = "\"" `append` pack s `append` "\""

-- | Selects an option if and only if it corresponds to a font attribute.
optToFont :: Option -> Maybe H.Attribute
optToFont (Color c)    = Just $ H.Color c
optToFont (FontFace s) = Just $ H.Face s
optToFont (FontSize d) = Just $ H.PointSize d
optToFont _            = Nothing

-- | Selects an option if and only if it corresponds to an HTML attribute.
-- In particular, for tables or table cells.
optToHtml :: Option -> Maybe H.Attribute
optToHtml (BgColor c)       = pure $ H.BGColor c
optToHtml (Border w)        = pure $ H.Border w
optToHtml (BorderColor c)   = pure $ H.Color c
optToHtml (CellSpacing w)   = pure $ H.CellSpacing w
optToHtml (CellBorder w)    = pure $ H.CellBorder w
optToHtml (CellPadding w)   = pure $ H.CellPadding w
optToHtml (TextAlignment x) = pure $ H.Align x
optToHtml _                 = Nothing


-- | Selects an option if and only if it corresponds to a label.
optToLabel :: Option -> Maybe Text
optToLabel (Label s) = Just $ pack s
optToLabel _         = Nothing

-- | Represents a relationship between exactly two entities. After parsing,
-- each `rel` is guaranteed to correspond to an entity defined in the same
-- ER file.
--
-- Each relationship has one of four cardinalities specified for both entities.
-- Those cardinalities are: 0 or 1, exactly 1, 0 or more and 1 or more.
data Relation = Relation
    { entity1, entity2 :: Text
    , card1, card2     :: Cardinality
    , roptions         :: Options
    }
    deriving (Show, Eq)

data Cardinality = ZeroOne
    | One
    | ZeroPlus
    | OnePlus
    deriving (Eq)

instance Show Cardinality where
  show ZeroOne  = "{0,1}"
  show One      = "1"
  show ZeroPlus = "0..N"
  show OnePlus  = "1..N"

-- | Maps a string representation to a particular relationship cardinality.
cardByName :: Char -> Maybe Cardinality
cardByName '?' = Just ZeroOne
cardByName '1' = Just One
cardByName '*' = Just ZeroPlus
cardByName '+' = Just OnePlus
cardByName _   = Nothing

-- | Hard-coded default options for all graph titles.
defaultTitleOpts :: Options
defaultTitleOpts = M.fromList
  [ ("size", FontSize 30)
  ]

-- | Hard-coded default options for all entity headers.
defaultHeaderOpts :: Options
defaultHeaderOpts = M.fromList
  [ ("size", FontSize 16)
  ]

-- | Hard-coded default options for all entities.
defaultEntityOpts :: Options
defaultEntityOpts = M.fromList
  [ ("border", Border 0)
  , ("cellborder", CellBorder 1)
  , ("cellspacing", CellSpacing 0)
  , ("cellpadding", CellPadding 4)
  , ("font", FontFace "Helvetica")
  ]

-- | Hard-coded default options for all relationships.
defaultRelOpts :: Options
defaultRelOpts = M.empty

defaultAttrOpts :: Options
defaultAttrOpts = M.fromList
  [ ("text-alignment", TextAlignment H.HLeft)
  ]
