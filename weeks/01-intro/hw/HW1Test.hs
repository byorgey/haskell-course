-- Modules needed:
--   test-framework
--   test-framework-hunit
--   test-framework-quickcheck2
--
-- should also pull in QuickCheck-2 and HUnit.

module HW1Test (main) where
import Control.Monad (when)
import qualified Data.Map as M

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import HW1Sol (Peg, Move, toDigitsRev, doubleEveryOther, sumDigits,
               validate, hanoi)

-- Test that a towers of Hanoi solution is valid. Validity means that
-- no move places a larger disc on top of a smaller disc, and that all
-- discs and up on the second peg.
testHanoi :: Integer -> [Peg] -> [Move] -> Bool
testHanoi n pegs moves = testHanoi' (head (tail pegs)) init moves
    where init = M.fromList $ (head pegs, [1..n]) : zip (tail pegs) (repeat [])

-- The state of a towers of Hanoi board is a mapping from pegs to the
-- discs stacked on those pegs.
type HanoiState = M.Map Peg [Integer]

-- Hanoi test helper that maintains a finite map from pegs to the
-- discs currently stacked on that peg. Checks each step of the moves,
-- and that the end state has no discs on any peg other than the
-- target peg.
testHanoi' :: Peg -> HanoiState -> [Move] -> Bool
testHanoi' dest pegs [] = and $ map okay (M.toList pegs)
    where okay (k,v) = k == dest || null v
testHanoi' dest pegs ((from,to):ms) = case tryMove pegs from to of
                                        Nothing -> False
                                        Just pegs' -> testHanoi' dest pegs' ms

-- Try moving a disc from one peg to another. If successful, return
-- the new state of the Hanoi board; otherwise return Nothing.
tryMove :: HanoiState -> Peg -> Peg -> Maybe HanoiState
tryMove pegs from to = do from' <- M.lookup from pegs
                          to' <- M.lookup to pegs
                          when (null from') Nothing
                          when (not (null to') && head from' > head to') Nothing
                          Just . M.adjust (head from' :) to . M.adjust tail from $ pegs

testHanoi3Valid = "testHanoi" ~: True ~=? solution
    where solution = testHanoi 3 ["a","b","c"] (hanoi 3 "a" "b" "c")

tests = [ testGroup "Credit card validation"
          [ testCase     "toDigitsRev 1"      test_toDigitsRev
          , testProperty "toDigitsRev 2"      prop_toDigitsRev

          , testCase     "doubleEveryOther 1" test_doubleEveryOther
          , testProperty "doubleEveryOther 2" prop_doubleEveryOther

          , testCase     "sumDigits 1"        test_sumDigits
          , testProperty "sumDigits 2"        prop_sumDigits

          , testCase     "validate 1"         test_validate_1
          , testCase     "validate 2"         test_validate_2
          , testCase     "validate 3"         test_validate_3
          ]

        , testGroup "Towers of hanoi" $
            flip map [1..7] $ \n ->
              testCase ("hanoi " ++ show n) (test_hanoi n)
        ]

-- Exercise 1
test_toDigitsRev = [4,3,2,1] @=? toDigitsRev 1234

myToDigits :: Integer -> [Integer]
myToDigits = map (read . return) . show

prop_toDigitsRev (Positive n) = toDigitsRev n == reverse (myToDigits n)

-- Exercise 2
test_doubleEveryOther = [16,7,12,5] @=? doubleEveryOther [8,7,6,5]

prop_doubleEveryOther ns = 
  zipWith div 
          (reverse.doubleEveryOther.reverse $ ns) 
          (cycle [1,2])
  == ns

-- Exercise 3
test_sumDigits = 20 @=? sumDigits [8,14,6,10]

prop_sumDigits :: [NonNegative Integer] -> Bool
prop_sumDigits ns = sumDigits ns' == sum (concatMap myToDigits ns')
  where ns' = map getNN ns
        getNN (NonNegative n) = n

-- Exercise 4
test_validate_1 = True @=? validate 4012888888881881
test_validate_2 = True @=? validate 2083947810098234
test_validate_3 = True @=? validate 9987405829873404

-- Exercise 5
test_hanoi n = (True @=? testHanoi n ["a","b","c"] (hanoi n "a" "b" "c"))


main = defaultMain tests