
--{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module HW5Test where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Control.Monad
import Data.List

import Calc
import qualified StackVM as S

main :: IO ()
main = defaultMain tests

tests = [ testGroup "Stack Program"
          [ testProperty "lit" prop_Program_lit 
          , testProperty "add" prop_Program_add
          , testProperty "mul" prop_Program_mul
          ]
        ]

newtype Prog = Prog S.Program deriving (Eq, Show)

instance Arbitrary Prog where
  arbitrary = liftM Prog $ arbProgram 5

arbProgram :: Int -> Gen S.Program
arbProgram 0 = (liftM ((:[]).S.PushI) arbitrary)
arbProgram n = oneof [ arbProgram 0
                     , liftM2 (\x y -> x ++ y ++ [S.Mul]) 
                              (arbProgram (n-1)) 
                              (arbProgram (n-1))
                     , liftM2 (\x y -> x ++ y ++ [S.Add]) 
                              (arbProgram (n-1)) 
                              (arbProgram (n-1))
                     ]

instance Eq S.StackExp where
  S.PushI x == S.PushI y = x == y
  S.PushB x == S.PushB y = x == y
  S.Add == S.Add = True
  S.Mul == S.Mul = True
  S.And == S.And = True
  S.Or == S.Or = True
  _ == _ = False

instance Expr Prog where
  lit x = Prog [S.PushI x]
  add (Prog p1) (Prog p2) = Prog (add p1 p2)
  mul (Prog p1) (Prog p2) = Prog (mul p1 p2)

prop_Program_lit :: Integer -> Bool
prop_Program_lit i = (lit i) == [S.PushI i]

prop_Program_add :: (Prog, Prog) -> Bool
prop_Program_add (Prog p1, Prog p2) = add p1 p2 == p1 ++ p2 ++ [S.Add]

prop_Program_mul :: (Prog, Prog) -> Bool
prop_Program_mul (Prog p1, Prog p2) = mul p1 p2 == p1 ++ p2 ++ [S.Mul]
