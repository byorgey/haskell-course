{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, StandaloneDeriving #-}

module HW10Test (tests, main) where

import Control.Applicative

import Data.Char

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import AParser

tests = [ testGroup "ex1/Functor"
          [ testCase "functor OK"   testFunctor1
          , testCase "functor fail" testFunctor2
          , testProperty "functor rand" prop_Functor
          ]

        , testGroup "ex2/Applicative"
          [ testCase "applicative OK" testApp1
          , testCase "applicative fail" testApp2
          , testCase "applicative pure" testAppPure
          , testProperty "applicative rand" prop_applicative
          ]
        , testGroup "ex3/Parsers"
          [ testCase "testAbParser1" testAbParser1
          , testCase "testAbParser2" testAbParser2
          , testCase "testAbParser3" testAbParser3
          , testCase "testAbParser4" testAbParser4
          , testCase "testAbParser_1" testAbParser_1
          , testCase "testAbParser_2" testAbParser_2
          , testCase "testAbParser_3" testAbParser_3
          , testCase "testAbParser_4" testAbParser_4
          , testCase "testIntPair1" testIntPair1
          , testCase "testIntPair2" testIntPair2
          , testCase "testIntPair3" testIntPair3
          , testCase "testIntPair4" testIntPair4
          ]
        , testGroup "ex4/Alternative"
          [ testCase "alternative1" testAlt1
          , testCase "alternative2" testAlt2
          , testCase "alternative3" testAlt3
          , testCase "alternative4" testAlt4
          , testCase "alternative5" testAlt5
          ]
        , testGroup "ex5/intOrUppercase"
          [ testCase "testIntOrUppercase1" testIntOrUppercase1
          , testCase "testIntOrUppercase2" testIntOrUppercase2
          , testCase "testIntOrUppercase3" testIntOrUppercase3
          , testCase "testIntOrUppercase4" testIntOrUppercase4
          ]
        ]

main = defaultMain tests

-- Exercise 1 / Functor

testFunctor1 = runParser (fmap (+2) posInt) "345" @?= Just (347, "")
testFunctor2 = runParser (fmap (+2) posInt) "bf4" @?= Nothing

prop_Functor :: Blind (Integer -> Integer) -> Positive Integer -> Bool
prop_Functor (Blind f) (Positive n) = runParser (fmap f posInt) (show n) == Just (f n, "")

-- Exercise 2 / Applicative

p2 = ((+) <$> posInt <*> (char ' ' *> posInt))

testApp1 = runParser p2 "345 678" @?= Just (1023, "")
testApp2 = runParser p2 "345 b6" @?= Nothing
testAppPure = runParser (pure 1) "test" @?= Just (1, "test")

prop_applicative :: Blind (Integer -> Integer -> Integer) -> Positive Integer -> Positive Integer -> Bool
prop_applicative (Blind f) (Positive m) (Positive n) =
  runParser (f <$> posInt <*> (char ' ' *> posInt)) (show m ++ " " ++ show n) == Just (f m n, "")

-- Exercise 3

testAbParser1 = runParser abParser "abcdef" @?= Just (('a', 'b'), "cdef")
testAbParser2 = runParser abParser "aebcdef" @?= Nothing
testAbParser3 = runParser abParser "ab" @?= Just (('a', 'b'), "")
testAbParser4 = runParser abParser " ab" @?= Nothing

testAbParser_1 = runParser abParser_ "abcdef" @?= Just ((), "cdef")
testAbParser_2 = runParser abParser_ "aebcdef" @?= Nothing
testAbParser_3 = runParser abParser_ "ab" @?= Just ((), "")
testAbParser_4 = runParser abParser_ " ab" @?= Nothing

testIntPair1 = runParser intPair "12 34" @?= Just ([12, 34], "")
testIntPair2 = runParser intPair "aebcdef" @?= Nothing
testIntPair3 = runParser intPair "123 456seven" @?= Just ([123, 456], "seven")
testIntPair4 = runParser intPair "1234" @?= Nothing



-- Exercise 4 / Alternative

p3 = posInt <|> pure 6
p4 = char 'x' <|> char 'y'

testAlt1 = runParser p3 "234" @?= Just (234, "")
testAlt2 = runParser p3 "x" @?= Just (6, "x")
testAlt3 = runParser p4 "x" @?= Just ('x', "")
testAlt4 = runParser p4 "y" @?= Just ('y', "")
testAlt5 = runParser p4 "z" @?= Nothing

-- Exercise 5

testIntOrUppercase1 = runParser intOrUppercase "342abcd" @?= Just ((), "abcd")
testIntOrUppercase2 = runParser intOrUppercase "XYZ" @?= Just ((), "YZ")
testIntOrUppercase3 = runParser intOrUppercase "foo" @?= Nothing
testIntOrUppercase4 = runParser intOrUppercase "..." @?= Nothing
