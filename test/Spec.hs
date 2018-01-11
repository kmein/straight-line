{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedLists
  #-}

import qualified Evaluator
import qualified Parser
import Types

import Data.Char (isNumber)
import Data.Maybe (fromJust)
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
                [ testProperty "no variables != o, i, x" $ \str ->
                      let (x:xs) = str
                      in not
                             (null str ||
                              str == "o" || x `elem` "ix" || all isNumber xs) ==>
                         parseMaybe Parser.variable str ==
                         Nothing
                , testCase "space irrelevant (1)" $
                  Just (Add Output Output) @=?
                  parseMaybe Parser.expression "o      +o"
                , testCase "space irrelevant (2)" $
                  Just (Assign Output (Constant 0)) @=?
                  parseMaybe Parser.instruction "o:=        0"
                , testCase "x0_ invalid" $
                  Nothing @=? (parseMaybe Parser.variable "x01")
                ]
          , testGroup
                "Pretty-Printer"
                [ testCase "(1)" $
                  "x0 := 1;\no := x0 * x0" @=?
                  show
                      (pretty $
                       Program
                           [ Assign (X 0) (Constant 1)
                           , Assign Output (Multiply (X 0) (X 0))
                           ])
                ]
          , testProperty "inverse" $ \(p :: Program) ->
                parseMaybe Parser.program (show $ pretty p) == Just p
          ]
    , let add = "o := i0 + i1"
          o0 = "o := 0"
          alwaysZero = "o := i0 * x0"
          product4 =
              "o := 1; o := o * i0; o := o * i1; o := o * i2; o := o * i3"
          parse = fromJust . parseMaybe Parser.program
      in testGroup
             "Evaluator"
             [ testProperty o0 $ \input -> 0 == Evaluator.run input (parse o0)
             , testProperty add $ \x y ->
                   x + y == Evaluator.run [x, y] (parse add)
             , testProperty alwaysZero $ \input ->
                   0 == Evaluator.run input (parse alwaysZero)
             , testProperty product4 $ \a b c d ->
                   a * b * c * d == Evaluator.run [a, b, c, d] (parse product4)
             , testProperty "i_ == 0" $ \prog ->
                   Evaluator.run (replicate 50 0) prog == Evaluator.run [] prog
             ]
    ]
