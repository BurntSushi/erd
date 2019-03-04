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

parseDoc :: Text -> (GlobalOptions, [AST]) -> Assertion
parseDoc input expect= Right expect `shouldBe` parse document "" (fromStrict input) where
  shouldBe = assertEqual ""

testEr :: TestTree
testEr = testGroup "Parse Er" [
  testCase "Parse Simple case" $ parseDoc simpleText simpleResult
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
