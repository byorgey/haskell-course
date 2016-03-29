

module HW5Test where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Control.Monad
import Data.List
import Data.Monoid

-- Provided
import Sized

-- Submission
import JoinList hiding (main)
import Scrabble

main = defaultMain tests

tests = [
          testGroup "append"
            [ testCase "product" testAppendProduct
            , testCase "sum" testAppendSum ],

          testGroup "indexJ"
            [ testCase "left" testIndexLeft
            , testCase "right" testIndexRight
            , testCase "empty" testIndexEmpty
            , testProperty "same" propIndexSame ],

          testGroup "dropJ"
            [ testCase "left" testDropLeft
            , testCase "right" testDropRight
            , testCase "empty" testDropEmpty
            , testProperty "same" propDropSame ],

          testGroup "takeJ"
            [ testCase "left" testTakeLeft
            , testCase "right" testTakeRight
            , testCase "empty" testTakeEmpty
            , testProperty "same" propTakeSame ],

          testGroup "scrabble"
            [ testCase "score" testScore ]

        ]

-- Exercise 1: Append

a1, b1, c1 :: JoinList (Product Int) String
a1 = Single (Product 3) "something"
b1 = Single (Product 5) "anotherthing"
c1 = Single (Product 7) "thelastthing"

testAppendProduct :: Assertion
testAppendProduct = (a1 +++ b1) +++ c1 @?=
                    Append (Product 105)
                      (Append (Product 15)
                        a1
                        b1)
                      c1

a2, b2, c2 :: JoinList (Sum Int) String
a2 = Single (Sum 3) "something"
b2 = Single (Sum 5) "anotherthing"
c2 = Single (Sum 7) "thelastthing"

testAppendSum :: Assertion
testAppendSum = (a2 +++ b2) +++ c2 @?=
                Append (Sum 15)
                  (Append (Sum 8)
                    a2
                    b2)
                  c2

-- Exercise 2: Sized indexJ, dropJ, takeJ

a3,b3,c3,d3,e3 :: JoinList Size String
a3 = Single (Size 1) "a"
b3 = Single (Size 1) "b"
c3 = Single (Size 1) "c"
d3 = Single (Size 1) "d"
e3 = Single (Size 1) "e"

left,right :: JoinList Size String

left = ((((a3 +++ b3) +++ c3) +++ d3) +++ e3)

right = (a3 +++ (b3 +++ (c3 +++ (d3 +++ e3))))

testIndexLeft :: Assertion
testIndexLeft = indexJ 2 left @?= Just "c"

testIndexRight :: Assertion
testIndexRight = indexJ 2 right @?= Just "c"

testIndexEmpty :: Assertion
testIndexEmpty = indexJ 2 (Empty :: JoinList Size String) @?= Nothing

propIndexSame :: Int -> Bool
propIndexSame i = indexJ i left == indexJ i right

testDropLeft :: Assertion
testDropLeft = dropJ 2 left @?= ((c3 +++ d3) +++ e3)

testDropRight :: Assertion
testDropRight = dropJ 2 right @?= (c3 +++ (d3 +++ e3))

testDropEmpty :: Assertion
testDropEmpty = dropJ 2 (Empty :: JoinList Size String) @?= Empty

propDropSame :: (Int,Int) -> Bool
propDropSame (x,y) = indexJ x (dropJ y left) ==
                     indexJ x (dropJ y right)

testTakeLeft :: Assertion
testTakeLeft = takeJ 3 left @?= ((a3 +++ b3) +++ c3)

testTakeRight :: Assertion
testTakeRight = takeJ 3 right @?= (a3 +++ (b3 +++ c3))

testTakeEmpty :: Assertion
testTakeEmpty = takeJ 2 (Empty :: JoinList Size String) @?= Empty

propTakeSame :: (Int,Int) -> Bool
propTakeSame (x,y) = indexJ x (takeJ y left) ==
                     indexJ x (takeJ y right)

-- Exercise 3: Scrabble Score

a4 :: JoinList Score String
a4 = scoreLine "yay " +++ scoreLine "haskell!"

testScore :: Assertion
testScore = (getScore . tag $ a4) @?= 23

-- Exercise 4: Buffer instance for (JoinList (Score, Size) String)

-- editor demo and code review
