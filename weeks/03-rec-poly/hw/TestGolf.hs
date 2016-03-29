{-# OPTIONS -Wall #-}

{-
 - Author:  Adi Dahiya <adahiya@seas.upenn.edu>
 - Date:    January 26, 2013
 -}
module TestGolf where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit

import Golf     (skips, localMaxima, histogram)
import qualified GolfSol  as Solution (skips, localMaxima, histogram)

tests = [ testGroup "Haskell Code Golf Exercises"
          [ testCase     "skips 1"      test_skips1
          , testCase     "skips 2"      test_skips2
          , testCase     "skips 3"      test_skips3
          , testCase     "skips 4"      test_skips4
          , testProperty "skips 5"      prop_skips

          , testCase     "maxima 1"     test_maxima1
          , testCase     "maxima 2"     test_maxima2
          , testCase     "maxima 3"     test_maxima3
          , testProperty "maxima 4"     prop_maxima

          , testCase     "histogram 1"  test_histogram1
          , testCase     "histogram 2"  test_histogram2
          , testProperty "histogram 3"  prop_histogram
          ]
        ]

-- Exercise 1
test_skips1, test_skips2, test_skips3, test_skips4 :: Assertion
test_skips1 = ["ABCD", "BD", "C", "D"] @=? skips "ABCD"
test_skips2 = ["hello!", "el!", "l!", "l", "o", "!"] @=? skips "hello!"
test_skips3 = [[1]] @=? skips [1]
test_skips4 = [[True, False], [False]] @=? skips [True, False]

prop_skips :: [Integer] -> Bool
prop_skips xs = skips xs == Solution.skips xs

-- Exercise 2
test_maxima1, test_maxima2, test_maxima3 :: Assertion
test_maxima1 = [9, 6] @=? localMaxima [2,9,5,6,1]
test_maxima2 = [4]    @=? localMaxima [2,3,4,1,5]
test_maxima3 = []     @=? localMaxima [1,2,3,4,5]

prop_maxima :: [Integer] -> Bool
prop_maxima xs = localMaxima xs == Solution.localMaxima xs

output1, output2 :: String
output1 = " *        \n *        \n *   *    \n==========\n0123456789\n"
output2 = "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"

-- Exercise 3
test_histogram1, test_histogram2 :: Assertion
test_histogram1 = output1 @=? histogram [1, 1, 1, 5]
test_histogram2 = output2 @=? histogram [1,4,5,4,6,6,3,4,2,4,9]

prop_histogram :: [Integer] -> Bool
prop_histogram xs = case filter (\x -> x >= 0 && x < 10) xs of
  [] -> True
  xs' -> histogram xs' == Solution.histogram xs'


main :: IO ()
main = defaultMain tests
