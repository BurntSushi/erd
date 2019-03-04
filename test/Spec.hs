module Main (main) where
import Test.Tasty
import Test.Text.Parsec.Erd.Parser (testEr)

main :: IO ()
main = defaultMain $ testGroup "Erd Tests"  tests

tests :: [TestTree]
tests = [testEr]
