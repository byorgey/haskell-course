{-# OPTIONS_GHC -fno-warn-orphans #-}

module HW8Test where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Control.Monad
import Data.Monoid

import Employee
import Party hiding (main)

main = defaultMain tests

tests = [
          testGroup "Guest List"
          [ testCase "glCons" testCons,
            testProperty "monoid laws" propMonoidLaws,
            testProperty "monoid fun" propMonoidFun,
            testProperty "ord fun" propOrdFun
          ],

          testCase "treeFold" testSumFun,

          testCase "nextLevel" testNextLevel,

          testCase "maxFun" testMaxFun

        ]

-- setup, instances, misc

e1,e2 :: Employee
e1 = Emp "e1" 10
e2 = Emp "e2" 7
e3 = Emp "e3" 4
e4 = Emp "e4" 6

gl0 :: GuestList
gl0 = mempty

gl1 :: GuestList
gl1 = e1 `glCons`
      (e2 `glCons`
      (e3 `glCons`
      (e4 `glCons` gl0)))

gl2 :: GuestList
gl2 = e2 `glCons` gl0

gl3 :: GuestList
gl3 = e3 `glCons` (e4 `glCons` gl0)

instance Arbitrary Employee where
  arbitrary = liftM2 Emp arbitrary (liftM abs arbitrary)

instance Arbitrary GuestList where
  arbitrary = liftM2 glCons arbitrary gl where
    gl = frequency [(4, arbitrary), (1, return $ GL [] 0)]

glGetFun :: GuestList -> Fun
glGetFun (GL _ f) = f

-- Exercise 1

testCons :: Assertion
testCons = glGetFun gl1 @=? 27

propMonoidLaws :: GuestList -> Bool
propMonoidLaws gl = gl Data.Monoid.<> mempty == gl
                  && mempty Data.Monoid.<> gl == gl

propMonoidFun :: (GuestList,GuestList) -> Bool
propMonoidFun (a, b) = glGetFun a + glGetFun b ==
  glGetFun (a Data.Monoid.<> b)

propOrdFun :: (GuestList,GuestList) -> Bool
propOrdFun (a,b) =
  (a <= b) == ((glGetFun a) <= (glGetFun b))

-- Exercise 2

simpleSumFun :: Employee -> [Fun] -> Fun
simpleSumFun = \e -> \fs -> empFun e + sum fs

testSumFun :: Assertion
testSumFun = treeFold simpleSumFun testCompany @=? 46

-- Exercise 3

testNextLevel :: Assertion
testNextLevel = yes == 20 && no == 10 @=? True where
  yes = glGetFun glYes
  no = glGetFun glNo
  (glYes, glNo) = nextLevel e1 [(gl2,gl3)]

-- Exercise 4

testMaxFun :: Assertion
testMaxFun = (glGetFun.maxFun $ testCompany) @=? 26
