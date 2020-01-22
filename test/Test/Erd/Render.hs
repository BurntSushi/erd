{-# LANGUAGE OverloadedStrings #-}

module Test.Erd.Render
  (testRender)
where

import qualified Erd.ER                        as ER
import           Erd.Render                    (htmlAttr)

import qualified Data.GraphViz.Attributes.HTML as H
import           Test.Tasty
import           Test.Tasty.HUnit

testRender :: TestTree
testRender = testGroup "Test of main-module" [
  testCase "Transform Erd field-attribute to HTML of GraphViz." test01
                                                 ]
test01 :: Assertion
test01 = result @?= expected
  where
    inputF   = "Field"
    input    = ER.Attribute inputF True False ER.defaultAttrOpts
    result   = htmlAttr input
    expected = H.Cells [H.LabelCell [H.Align H.HLeft]
                        (H.Text [H.Format H.Underline [H.Font [] [H.Str inputF]]])]
