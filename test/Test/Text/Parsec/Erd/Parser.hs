{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Text.Parsec.Erd.Parser
  (testEr)
  where
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)
import Data.Text  (Text)
import Data.Text.Lazy (fromStrict)
import Text.Parsec (parse)
import qualified Data.Map as M
import Data.Map (fromList)
import Erd.ER
import Text.Parsec.Erd.Parser (document, AST(..), GlobalOptions(..))
import Data.GraphViz.Attributes.Colors (Color(..))

parseDoc :: Text -> (GlobalOptions, [AST]) -> Assertion
parseDoc input expect= Right expect `shouldBe` parse document "" (fromStrict input) where
  shouldBe = assertEqual ""

testEr :: TestTree
testEr = testGroup "Parse Er" [
  testCase "Parse Simple case" $ parseDoc simpleText simpleResult,
  testCase "Parse nfldb case" $ parseDoc nfldbText nfldbResult
                              ]


simpleText :: Text
simpleText = [r|
# Entities are declared in '[' ... ']'. All attributes after the entity header
# up until the end of the file (or the next entity declaration) correspond
# to this entity.
[Person]
*name
height
weight
`birth date`
+birth_place_id

[`Birth Place`]
*id
`birth city`
'birth state'
"birth country"

# Each relationship must be between exactly two entities, which need not
# be distinct. Each entity in the relationship has exactly one of four
# possible cardinalities:
#
# Cardinality    Syntax
# 0 or 1         ?
# exactly 1      1
# 0 or more      *
# 1 or more      +
Person *--1 `Birth Place`
|]

simpleResult :: (GlobalOptions, [AST])
simpleResult = (opts, asts) where
  opts = GlobalOptions M.empty M.empty M.empty M.empty
  asts = [
    E (Entity {name = "Person", attribs = [], hoptions = fromList [], eoptions = fromList []}),
    A (Attribute {field = "name", pk = True, fk = False, aoptions = fromList []}),
    A (Attribute {field = "height", pk = False, fk = False, aoptions = fromList []}),
    A (Attribute {field = "weight", pk = False, fk = False, aoptions = fromList []}),
    A (Attribute {field = "birth date", pk = False, fk = False, aoptions = fromList []}),
    A (Attribute {field = "birth_place_id", pk = False, fk = True, aoptions = fromList []}),
    E (Entity {name = "Birth Place", attribs = [], hoptions = fromList [], eoptions = fromList []}),
    A (Attribute {field = "id", pk = True, fk = False, aoptions = fromList []}),
    A (Attribute {field = "birth city", pk = False, fk = False, aoptions = fromList []}),
    A (Attribute {field = "birth state", pk = False, fk = False, aoptions = fromList []}),
    A (Attribute {field = "birth country", pk = False, fk = False, aoptions = fromList []}),
    R (Relation {entity1 = "Person", entity2 = "Birth Place", card1 = ZeroPlus , card2 = One, roptions = fromList []})]

nfldbText :: Text
nfldbText = [r|
title {label: "nfldb Entity-Relationship diagram (condensed)", size: "20"}

# Nice colors from Erwiz:
# red #fcecec
# blue #ececfc
# green #d0e0d0
# yellow #fbfbdb
# orange #eee0a0

# Entities

[player] {bgcolor: "#d0e0d0"}
  *player_id {label: "varchar, not null"}
  full_name {label: "varchar, null"}
  team {label: "varchar, not null"}
  position {label: "player_pos, not null"}
  status {label: "player_status, not null"}

[team] {bgcolor: "#d0e0d0"}
  *team_id {label: "varchar, not null"}
  city {label: "varchar, not null"}
  name {label: "varchar, not null"}

[game] {bgcolor: "#ececfc"}
  *gsis_id {label: "gameid, not null"}
  start_time {label: "utctime, not null"}
  week {label: "usmallint, not null"}
  season_year {label: "usmallint, not null"}
  season_type {label: "season_phase, not null"}
  finished {label: "boolean, not null"}
  home_team {label: "varchar, not null"}
  home_score {label: "usmallint, not null"}
  away_team {label: "varchar, not null"}
  away_score {label: "usmallint, not null"}

[drive] {bgcolor: "#ececfc"}
  *+gsis_id {label: "gameid, not null"}
  *drive_id {label: "usmallint, not null"}
  start_field {label: "field_pos, null"}
  start_time {label: "game_time, not null"}
  end_field {label: "field_pos, null"}
  end_time {label: "game_time, not null"}
  pos_team {label: "varchar, not null"}
  pos_time {label: "pos_period, null"}

[play] {bgcolor: "#ececfc"}
  *+gsis_id {label: "gameid, not null"}
  *+drive_id {label: "usmallint, not null"}
  *play_id {label: "usmallint, not null"}
  time {label: "game_time, not null"}
  pos_team {label: "varchar, not null"}
  yardline {label: "field_pos, null"}
  down {label: "smallint, null"}
  yards_to_go {label: "smallint, null"}

[play_player] {bgcolor: "#ececfc"}
  *+gsis_id {label: "gameid, not null"}
  *+drive_id {label: "usmallint, not null"}
  *+play_id {label: "usmallint, not null"}
  *+player_id {label: "varchar, not null"}
  team {label: "varchar, not null"}

[meta] {bgcolor: "#fcecec"}
  version {label: "smallint, null"}
  season_type {label: "season_phase, null"}
  season_year {label: "usmallint, null"}
  week {label: "usmallint, null"}

# Relationships

player      *--1 team
game        *--1 team {label: "home"}
game        *--1 team {label: "away"}
drive       *--1 team
play        *--1 team
play_player *--1 team

game        1--* drive
game        1--* play
game        1--* play_player

drive       1--* play
drive       1--* play_player

play        1--* play_player

player      1--* play_player
|]


data ChunckAST = CE [Entity] | CA [Attribute] | CR [Relation] deriving (Eq)
toAST :: ChunckAST -> [AST]
toAST (CE x) = map E x
toAST (CA x) = map A x
toAST (CR x) = map R x

nfldbResult :: (GlobalOptions, [AST])
nfldbResult = (opts, asts) where
  opts = GlobalOptions {gtoptions = fromList [("label",Label "nfldb Entity-Relationship diagram (condensed)"),("size",FontSize 20.0)], ghoptions = fromList [], geoptions = fromList [], groptions = fromList []}
  asts = concatMap toAST $ entities:(attributes ++ [relations])
  entities = CE [
    Entity {name = "player", attribs = [],
      hoptions = fromList [("bgcolor",BgColor (RGB {red = 208, green = 224, blue = 208}))],
      eoptions = fromList [("bgcolor",BgColor (RGB {red = 208, green = 224, blue = 208}))]
           }
    ]
  attributes = [
    CA [
        Attribute {field = "player_id", pk = True, fk = False, aoptions = fromList [("label",Label "varchar, not null")]},
        Attribute {field = "full_name", pk = False, fk = False, aoptions = fromList [("label",Label "varchar, null")]},
        Attribute {field = "team", pk = False, fk = False, aoptions = fromList [("label",Label "varchar, not null")]},
        Attribute {field = "position", pk = False, fk = False, aoptions = fromList [("label",Label "player_pos, not null")]},
        Attribute {field = "status", pk = False, fk = False, aoptions = fromList [("label",Label "player_status, not null")]}
      ],
    CE [
        Entity {name = "team", attribs = [], hoptions = fromList [("bgcolor",BgColor (RGB {red = 208, green = 224, blue = 208}))], eoptions = fromList [("bgcolor",BgColor (RGB {red = 208, green = 224, blue = 208}))]}
      ],
    CA [
        Attribute {field = "team_id", pk = True, fk = False, aoptions = fromList [("label",Label "varchar, not null")]},
        Attribute {field = "city", pk = False, fk = False, aoptions = fromList [("label",Label "varchar, not null")]},
        Attribute {field = "name", pk = False, fk = False, aoptions = fromList [("label",Label "varchar, not null")]}
      ],
    CE [
        Entity {name = "game", attribs = [], hoptions = fromList [("bgcolor",BgColor (RGB {red = 236, green = 236, blue = 252}))], eoptions = fromList [("bgcolor",BgColor (RGB {red = 236, green = 236, blue = 252}))]}
      ],
    CA [
        Attribute {field = "gsis_id", pk = True, fk = False, aoptions = fromList [("label",Label "gameid, not null")]},
        Attribute {field = "start_time", pk = False, fk = False, aoptions = fromList [("label",Label "utctime, not null")]},
        Attribute {field = "week", pk = False, fk = False, aoptions = fromList [("label",Label "usmallint, not null")]},
        Attribute {field = "season_year", pk = False, fk = False, aoptions = fromList [("label",Label "usmallint, not null")]},
        Attribute {field = "season_type", pk = False, fk = False, aoptions = fromList [("label",Label "season_phase, not null")]},
        Attribute {field = "finished", pk = False, fk = False, aoptions = fromList [("label",Label "boolean, not null")]},
        Attribute {field = "home_team", pk = False, fk = False, aoptions = fromList [("label",Label "varchar, not null")]},
        Attribute {field = "home_score", pk = False, fk = False, aoptions = fromList [("label",Label "usmallint, not null")]},
        Attribute {field = "away_team", pk = False, fk = False, aoptions = fromList [("label",Label "varchar, not null")]},
        Attribute {field = "away_score", pk = False, fk = False, aoptions = fromList [("label",Label "usmallint, not null")]}
      ],
    CE [
        Entity {name = "drive", attribs = [], hoptions = fromList [("bgcolor",BgColor (RGB {red = 236, green = 236, blue = 252}))], eoptions = fromList [("bgcolor",BgColor (RGB {red = 236, green = 236, blue = 252}))]}
      ],
    CA [
        Attribute {field = "gsis_id", pk = True, fk = True, aoptions = fromList [("label",Label "gameid, not null")]},
        Attribute {field = "drive_id", pk = True, fk = False, aoptions = fromList [("label",Label "usmallint, not null")]},
        Attribute {field = "start_field", pk = False, fk = False, aoptions = fromList [("label",Label "field_pos, null")]},
        Attribute {field = "start_time", pk = False, fk = False, aoptions = fromList [("label",Label "game_time, not null")]},
        Attribute {field = "end_field", pk = False, fk = False, aoptions = fromList [("label",Label "field_pos, null")]},
        Attribute {field = "end_time", pk = False, fk = False, aoptions = fromList [("label",Label "game_time, not null")]},
        Attribute {field = "pos_team", pk = False, fk = False, aoptions = fromList [("label",Label "varchar, not null")]},
        Attribute {field = "pos_time", pk = False, fk = False, aoptions = fromList [("label",Label "pos_period, null")]}
      ],
    CE [
        Entity {name = "play", attribs = [], hoptions = fromList [("bgcolor",BgColor (RGB {red = 236, green = 236, blue = 252}))], eoptions = fromList [("bgcolor",BgColor (RGB {red = 236, green = 236, blue = 252}))]}
      ],
    CA [
        Attribute {field = "gsis_id", pk = True, fk = True, aoptions = fromList [("label",Label "gameid, not null")]},
        Attribute {field = "drive_id", pk = True, fk = True, aoptions = fromList [("label",Label "usmallint, not null")]},
        Attribute {field = "play_id", pk = True, fk = False, aoptions = fromList [("label",Label "usmallint, not null")]},
        Attribute {field = "time", pk = False, fk = False, aoptions = fromList [("label",Label "game_time, not null")]},
        Attribute {field = "pos_team", pk = False, fk = False, aoptions = fromList [("label",Label "varchar, not null")]},
        Attribute {field = "yardline", pk = False, fk = False, aoptions = fromList [("label",Label "field_pos, null")]},
        Attribute {field = "down", pk = False, fk = False, aoptions = fromList [("label",Label "smallint, null")]},
        Attribute {field = "yards_to_go", pk = False, fk = False, aoptions = fromList [("label",Label "smallint, null")]}
      ],
    CE [
        Entity {name = "play_player", attribs = [], hoptions = fromList [("bgcolor",BgColor (RGB {red = 236, green = 236, blue = 252}))], eoptions = fromList [("bgcolor",BgColor (RGB {red = 236, green = 236, blue = 252}))]}
      ],
    CA [
        Attribute {field = "gsis_id", pk = True, fk = True, aoptions = fromList [("label",Label "gameid, not null")]},
        Attribute {field = "drive_id", pk = True, fk = True, aoptions = fromList [("label",Label "usmallint, not null")]},
        Attribute {field = "play_id", pk = True, fk = True, aoptions = fromList [("label",Label "usmallint, not null")]},
        Attribute {field = "player_id", pk = True, fk = True, aoptions = fromList [("label",Label "varchar, not null")]},
        Attribute {field = "team", pk = False, fk = False, aoptions = fromList [("label",Label "varchar, not null")]}
     ],
     CE [
        Entity {name = "meta", attribs = [], hoptions = fromList [("bgcolor",BgColor (RGB {red = 252, green = 236, blue = 236}))], eoptions = fromList [("bgcolor",BgColor (RGB {red = 252, green = 236, blue = 236}))]}
       ],
    CA [
        Attribute {field = "version", pk = False, fk = False, aoptions = fromList [("label",Label "smallint, null")]},
        Attribute {field = "season_type", pk = False, fk = False, aoptions = fromList [("label",Label "season_phase, null")]},
        Attribute {field = "season_year", pk = False, fk = False, aoptions = fromList [("label",Label "usmallint, null")]},
        Attribute {field = "week", pk = False, fk = False, aoptions = fromList [("label",Label "usmallint, null")]}
      ]
    ]
  relations = CR [
     Relation {entity1 = "player", entity2 = "team", card1 = ZeroPlus, card2 = One, roptions = fromList []},
     Relation {entity1 = "game", entity2 = "team", card1 = ZeroPlus, card2 = One, roptions = fromList [("label",Label "home")]},
     Relation {entity1 = "game", entity2 = "team", card1 = ZeroPlus, card2 = One, roptions = fromList [("label",Label "away")]},
     Relation {entity1 = "drive", entity2 = "team", card1 = ZeroPlus, card2 = One, roptions = fromList []},
     Relation {entity1 = "play", entity2 = "team", card1 = ZeroPlus, card2 = One, roptions = fromList []},
     Relation {entity1 = "play_player", entity2 = "team", card1 = ZeroPlus, card2 = One, roptions = fromList []},
     Relation {entity1 = "game", entity2 = "drive", card1 = One, card2 = ZeroPlus, roptions = fromList []},
     Relation {entity1 = "game", entity2 = "play", card1 = One, card2 = ZeroPlus, roptions = fromList []},
     Relation {entity1 = "game", entity2 = "play_player", card1 = One, card2 = ZeroPlus, roptions = fromList []},
     Relation {entity1 = "drive", entity2 = "play", card1 = One, card2 = ZeroPlus, roptions = fromList []},
     Relation {entity1 = "drive", entity2 = "play_player", card1 = One, card2 = ZeroPlus, roptions = fromList []},
     Relation {entity1 = "play", entity2 = "play_player", card1 = One, card2 = ZeroPlus, roptions = fromList []},
     Relation {entity1 = "player", entity2 = "play_player", card1 = One, card2 = ZeroPlus, roptions = fromList []}
               ]
