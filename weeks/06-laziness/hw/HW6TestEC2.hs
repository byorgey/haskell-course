{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW6TestEC2 (tests, main) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import qualified HW6Test as HW6
import Fibonacci 
  ( fib4 )

main = defaultMain (HW6.tests ++ tests)

tests = [ testGroup "ex7"
          [ testCase "fib4_zero" testFib4_zero
          , testCase "fib4_small" testFib4_small
          , testProperty "fib4" propFib4
          , testCase "fib4_big" testFib4_big
          ]
        ]

testFib4_zero = fib4 0 @=? 0
testFib4_small  = map fib4 [1..10] @=? [1,1,2,3,5,8,13,21,34,55]

propFib4 :: Positive Integer -> Bool
propFib4 (Positive n) = fib4 n + fib4 (n+1) == fib4 (n+2)

testFib4_big = assert (fib4 10000000 > 1)