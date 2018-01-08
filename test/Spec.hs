{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
import Types
import qualified Parser

import Data.Char (isNumber)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Parsec (parse)
import Text.PrettyPrint.Leijen (pretty)

main :: IO ()
main = defaultMain $ testGroup "Tests" tests

parseMaybe p = either (const Nothing) Just . parse p "(test)"

tests :: [TestTree]
tests =
    [ testGroup
          "Parser / Pretty-Printer"
          [ testGroup
                "Parser"
                [ testProperty "no variables != o, i, x" $
                  \str ->
                       let (x:xs) = str
                       in not
                              (null str ||
                               str == "o" || x `elem` "ix" || all isNumber xs) ==>
                          parseMaybe Parser.variable str ==
                          Nothing
                , testCase
                      "space irrelevant (1)"
                      (Just (Add Output Output) @=?
                       parseMaybe Parser.expression "o      +o")
                , testCase
                      "space irrelevant (2)"
                      (Just (Assign Output (Constant 0)) @=?
                       parseMaybe Parser.instruction "o:=        0")
                , testCase
                      "x0_ invalid"
                      (Nothing @=? (parseMaybe Parser.variable "x01"))]
          , testGroup
                "Pretty-Printer"
                [ testCase "(1)" $
                  "x0 := 1;\no := x0 * x0" @=?
                  show
                      (pretty $
                       Cons
                           (Assign (X 0) (Constant 1))
                           (Cons (Assign Output (Multiply (X 0) (X 0))) Nil))]
          , testProperty "inverse" $
            \(p :: Program) ->
                 parseMaybe Parser.program (show $ pretty p) == Just p]]
