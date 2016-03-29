{-# OPTIONS -Wall #-}

{-
 - Author:  Zachary Wasserman <zwass@seas.upenn.edu>
 - Date:    February 11, 2013
 -}
module HW4Test where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck.Function

import Data.List ((\\))

import qualified HW4Sol as Solution (fun1, fun2, xor, sieveSundaram)
import HW4 (Tree(..), fun1', fun2', foldTree, xor, map', sieveSundaram)

tests = [ testGroup "CIS194 HW4 Exercises"
          [ -- Exercise 1
            testProperty "fun1" prop_fun1
          , testCase "fun2_1" test_fun21
          , testCase "fun2_2" test_fun22
          , testCase "fun2_3" test_fun23
          , testCase "fun2_4" test_fun24

            -- Exercise 2
          , testProperty "foldTree elements" prop_elements
          , testProperty "foldTree balanced" prop_balanced
          , testProperty "foldTree heights" prop_heights

            -- Exercise 3
          , testCase "xor_1" test_xor1
          , testCase "xor_2" test_xor2
          , testProperty "xor" prop_xor
          , testProperty "map'" prop_map'

            -- Exercise 4
          , testProperty "sieveSundaram" prop_sieveSundaram
          ]
        ]

-- Exercise 1
prop_fun1 :: [Integer] -> Bool
prop_fun1 xs = fun1' xs == Solution.fun1 xs


test_fun2 :: Integer -> Assertion
test_fun2 x = Solution.fun2 x @=? fun2' x

test_fun21, test_fun22, test_fun23, test_fun24 :: Assertion
test_fun21 = test_fun2 1
test_fun22 = test_fun2 3
test_fun23 = test_fun2 6
test_fun24 = test_fun2 13


-- Exercise 2
treeFold :: (Integer -> b -> a -> b -> b) -> b -> Tree a -> b
treeFold _ acc Leaf = acc
treeFold f acc (Node h l x r) = f h (treeFold f acc l) x (treeFold f acc r)

-- Verify that the elements stay the same after the fold
prop_elements :: [Int] -> Bool
prop_elements xs =
  let tree = foldTree xs in
  let elements = treeFold (\_ l x r -> l ++ [x] ++ r) [] tree in
  null (xs \\ elements) && null (elements \\ xs)

-- Verify that the tree is balanced
prop_balanced :: [Int] -> Bool
prop_balanced xs =
  fst $ treeFold foldBalancedFun (True, 0) tree where
    tree = foldTree xs
    foldBalancedFun :: Integer -> (Bool, Int) -> a -> (Bool, Int)
                       -> (Bool, Int)
    foldBalancedFun _ (leftOk, leftH) _ (rightOk, rightH) =
      (leftOk && rightOk && abs (leftH - rightH) <= 1, max leftH rightH + 1)

-- Verify that the heights are correct
prop_heights :: [Int] -> Bool
prop_heights xs =
  fst $ treeFold foldHeightsFun (True, -1) tree where
    tree = foldTree xs
    foldHeightsFun :: Integer -> (Bool, Integer) -> a -> (Bool, Integer)
                      -> (Bool, Integer)
    foldHeightsFun h (leftOk, leftH) _ (rightOk, rightH) =
      (leftOk && rightOk && h == (max leftH rightH) + 1, h)


-- Exercise 3
test_xor :: [Bool] -> Assertion
test_xor xs = Solution.xor xs @=? xor xs

test_xor1, test_xor2 :: Assertion
test_xor1 = test_xor [False, True, False]
test_xor2 = test_xor [False, True, False, False, True]

prop_xor :: [Bool] -> Bool
prop_xor xs = xor xs == Solution.xor xs

prop_map' :: (Fun Int Int) -> [Int] -> Bool
prop_map' (Fun _ f) xs = map' f xs == map f xs

-- Exercise 4

prop_sieveSundaram :: Integer -> Bool
prop_sieveSundaram x = sieveSundaram x == Solution.sieveSundaram x

main :: IO ()
main = defaultMain tests
