{-# LANGUAGE OverloadedStrings #-}

module Test.Erd.Render
  (testRender)
where

<<<<<<< HEAD
import qualified Erd.ER as ER
import Erd.Render (htmlAttr)

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Attributes.HTML as H
import qualified Data.Map as M
=======
import qualified Erd.ER                        as ER
import           Erd.Render                    (htmlAttr)

import qualified Data.GraphViz.Attributes.HTML as H
import qualified Data.Map                      as M
import           Test.Tasty
import           Test.Tasty.HUnit
>>>>>>> master

testRender :: TestTree
testRender = testGroup "Test of main-module" [
  testCase "Transform Erd field-attribute to HTML of GraphViz." test01
                                                 ]
test01 :: Assertion
test01 = result @?= expected
  where
<<<<<<< HEAD
    inputF   = "id spaced Field"
    input    = ER.Attribute inputF True False M.empty
    result   = htmlAttr input
    expected = H.Cells [H.LabelCell [H.Align H.HRight,
                                     H.Port (A.PN {A.portName = "id_spaced_field"})
                                    ]
=======
    inputF   = "Field"
    input    = ER.Attribute inputF True False M.empty
    result   = htmlAttr input
    expected = H.Cells [H.LabelCell [H.Align H.HRight]
>>>>>>> master
                        (H.Text [H.Format H.Underline [H.Font [] [H.Str inputF]]])]
