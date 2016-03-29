

module HW5Test where

import Data.Map as M

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Control.Monad
import Data.List

import Calc
import ExprT
import Parser

main = defaultMain tests

tests = [ testGroup "part 6"
          [ testCase "1" test_1
          , testCase "2" test_2
          , testCase "3" test_3
          , testCase "4" test_4
          ]
        ]

test_1, test_2, test_3, test_4 :: Assertion

test_1 = Just 9 @=? (ourWithVars [("x", 6)] $ add (lit 3) (var "x"))

test_2 = Nothing @=? (ourWithVars [("x", 6)] $ add (lit 3) (var "y"))

test_3 = Just 54 @=?
         (ourWithVars [("x", 6), ("y", 3)]
          $ mul (var "x") (add (var "y") (var "x")))

test_4 = Just 90 @=?
         (ourWithVars [("x", 6), ("y", 3), ("z", 4)]
          $ mul (add (var "x") (var "z")) (add (var "y") (var "x")))

ourWithVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
ourWithVars vs exp = exp $ M.fromList vs
