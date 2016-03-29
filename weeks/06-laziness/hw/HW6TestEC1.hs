{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW6TestEC1 (tests, main) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import qualified HW6Test as HW6
import HW6Test ((===), theFibs)
import Fibonacci 
  ( streamToList, x, fibs3 )

main = defaultMain (HW6.tests ++ tests)

tests = [ testGroup "ex6"
          [ testCase "x" testX
          , testProperty "fromInteger" propFromInteger
          , testProperty "binom" propBinom
          , testCase "fibs3" testFibs3
          ]
        ]

testX = assert (streamToList x === [0,1] ++ repeat 0)

propFromInteger :: Integer -> Bool
propFromInteger = streamToList . fromInteger === (:repeat 0)

propBinom :: Integer -> Positive Integer -> Bool
propBinom k (Positive n) = streamToList ((x + fromInteger k)^n) === map (\i -> binom n i * k^(n-i)) [0..n] ++ repeat 0

binom n k
  | k > n     = 0
  | otherwise = fac n `div` (fac k * fac (n-k))
                
fac n = product [2..n]

testFibs3 = assert (streamToList fibs3 === theFibs)