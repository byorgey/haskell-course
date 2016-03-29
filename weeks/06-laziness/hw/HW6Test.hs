{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module HW6Test (tests, main, (===), theFibs) where

import Control.Applicative ((<$>))

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Fibonacci 
  ( Stream, fib, fibs1, fibs2
  , streamToList, streamRepeat, streamMap
  , streamFromSeed, nats, ruler
  )

tests = [ testGroup "ex1"
          [ testCase "naive fib small" testFibSmall
          , testCase "naive fibs1 small" testFibs1Small
          ]
          
        , testGroup "ex2"
          [ testCase "better fib 1000" testFibs2
          ]
          
        , testGroup "ex34"
          [ testProperty "repeat" prop_repeat
          , testProperty "map"    prop_map
          , testProperty "seed"   prop_seed
          ]
          
        , testGroup "ex5"
          [ testCase "nats" testNats
          , testProperty "ruler" propRuler
          ]
        ]

theFibs = 0 : 1 : zipWith (+) theFibs (tail theFibs)

-- Exercise 1

testFibSmall =
  map fib [0..10] @=? take 11 theFibs

testFibs1Small =
  take 10 fibs1 @=? take 10 theFibs

-- Exercise 2

testFibs2 =
  assert (take 1000 fibs2 == take 1000 theFibs)

main = defaultMain tests

-- Exercise 3/4/5

infix 4 ===
class StreamComparable s where
  type SCRes s :: *
  (===) :: s -> s -> SCRes s
  
instance Eq a => StreamComparable [a] where
  type SCRes [a] = Bool
  l1 === l2 = take 20 l1 == take 20 l2
  
instance Eq a => StreamComparable (Stream a) where
  type SCRes (Stream a) = Bool
  s1 === s2 = streamToList s1 === streamToList s2
  
instance StreamComparable s => StreamComparable (a -> s) where
  type SCRes (a -> s) = a -> SCRes s
  f1 === f2 = \x -> f1 x === f2 x
  
prop_repeat :: Int -> Bool
prop_repeat = streamToList . streamRepeat === repeat

prop_map :: Int -> Bool
prop_map = streamToList . streamMap (+1) . streamRepeat === repeat . (+1)

prop_seed :: Blind (Int -> Int) -> Int -> Bool
prop_seed (Blind f) = streamToList . streamFromSeed f === iterate f

testNats = assert ([0..1999] == take 2000 (streamToList nats))

newtype SmallInt = SI { getSI :: Int }
  deriving (Eq, Ord, Num, Show)

instance Arbitrary SmallInt where
  arbitrary = (SI . (`mod` 500)) <$> arbitrary

rulerL = streamToList ruler

propRuler :: SmallInt -> Bool
propRuler (SI i) = rulerL !! i == rulerF (i+1)

rulerF n | odd n     = 0
         | otherwise = 1 + rulerF (n `div` 2)