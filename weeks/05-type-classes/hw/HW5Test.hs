

module HW5Test where

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

tests = [ testGroup "eval"
          [ testCase "eval unit" test_eval1
          , testProperty "eval check" prop_eval
          ]

        , testGroup "evalStr" $
          map (\(lbl,assert) -> testCase ("evalStr " ++ show lbl) assert)
              (zip [1..] evalStr_tests)

        , testGroup "instance ExprT"
          [ testProperty "lit" prop_ExprT_lit
          , testProperty "add" prop_ExprT_add
          , testProperty "mul" prop_ExprT_mul
          ]

        , testGroup "Integer"
          [ testProperty "lit" prop_Integer_lit
          , testProperty "add" prop_Integer_add
          , testProperty "mul" prop_Integer_mul
          ]

        , testGroup "Bool"
          [ testProperty "lit" prop_Bool_lit
          , testProperty "add" prop_Bool_add
          , testProperty "mul" prop_Bool_mul
          ]

        , testGroup "MinMax"
          [ testProperty "lit" prop_MinMax_lit
          , testProperty "add" prop_MinMax_add
          , testProperty "mul" prop_MinMax_mul
          ]

        , testGroup "Mod7"
          [ testProperty "lit" prop_Mod7_lit
          , testProperty "add" prop_Mod7_add
          , testProperty "mul" prop_Mod7_mul
          ]


        ]

-- Exercise 1

test_eval1 :: Assertion
test_eval1 = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) @=? 20

eval' :: ExprT -> Integer
eval' (Lit i) = i
eval' (Add x y) = eval' x + eval' y
eval' (Mul x y) = eval' x * eval' y

prop_eval :: ExprT -> Bool
prop_eval e = eval e == eval' e

-- Exercise 2

evalStr_tests :: [Assertion]
evalStr_tests = [ evalStr "(2+3)*4" @=? Just 20
                , evalStr "2+3*4" @=? Just 14
                , evalStr "2+3*" @=? Nothing ]

-- Exercise 3

instance Arbitrary ExprT where
  arbitrary = arbExprT 5

arbExprT :: Int -> Gen ExprT
arbExprT 0 = liftM Lit arbitrary
arbExprT n = oneof [ arbExprT 0
                   , liftM2 Mul (arbExprT (n-1)) (arbExprT (n-1))
                   , liftM2 Add (arbExprT (n-1)) (arbExprT (n-1)) ]

prop_ExprT_lit :: Integer -> Bool
prop_ExprT_lit i = lit i == Lit i

prop_ExprT_add :: (ExprT, ExprT) -> Bool
prop_ExprT_add (e1, e2) = add e1 e2 == Add e1 e2

prop_ExprT_mul :: (ExprT, ExprT) -> Bool
prop_ExprT_mul (e1, e2) = mul e1 e2 == Mul e1 e2

-- Exercise 4

prop_Integer_lit :: Integer -> Bool
prop_Integer_lit i = lit i == i

prop_Integer_add :: (Integer, Integer) -> Bool
prop_Integer_add (x, y) = add x y == x + y

prop_Integer_mul :: (Integer, Integer) -> Bool
prop_Integer_mul (x, y) = mul x y == x * y

prop_Bool_lit :: Integer -> Bool
prop_Bool_lit x = lit x == (x > 0)

prop_Bool_add :: (Bool, Bool) -> Bool
prop_Bool_add (x, y) = add x y == (x || y)

prop_Bool_mul :: (Bool, Bool) -> Bool
prop_Bool_mul (x, y) = mul x y == (x && y)

{-
instance Eq MinMax where
  MinMax x == MinMax y = x == y
-}

instance Arbitrary MinMax where
  arbitrary = liftM MinMax arbitrary

prop_MinMax_lit :: Integer -> Bool
prop_MinMax_lit i = lit i == MinMax i

prop_MinMax_add :: (MinMax, MinMax) -> Bool
prop_MinMax_add (a@(MinMax x), b@(MinMax y)) = add a b == (lit $ max x y)

prop_MinMax_mul :: (MinMax, MinMax) -> Bool
prop_MinMax_mul (a@(MinMax x), b@(MinMax y)) = mul a b == (lit $ min x y)

{-
instance Eq Saturated where
  Saturated x == Saturated y = x == y
-}

instance Arbitrary Mod7 where
  arbitrary = liftM (Mod7 . (`mod` 7)) arbitrary

prop_Mod7_lit :: Integer -> Bool
prop_Mod7_lit i = case lit i of
  Mod7 n -> n == i `mod` 7

prop_Mod7_add :: (Mod7, Mod7) -> Bool
prop_Mod7_add (a@(Mod7 x), b@(Mod7 y)) = add a b == lit (x + y)

prop_Mod7_mul :: (Mod7, Mod7) -> Bool
prop_Mod7_mul (a@(Mod7 x), b@(Mod7 y)) = mul a b == lit (x * y)
