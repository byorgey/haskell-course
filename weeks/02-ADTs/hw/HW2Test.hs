-- Modules needed:
--   test-framework
--   test-framework-hunit
--   test-framework-quickcheck2
--
-- should also pull in QuickCheck-2 and HUnit.

module HW2Test (main) where
import Control.Monad (when)
import qualified Data.Map as M

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import LogAnalysis (parseMessage, parse, whatWentWrong)
import Log

tests = [ testGroup "Homework 2 tests"
          [ testCase     "parseMessage/1"     test_parseMessage_1
          , testCase     "parseMessage/2"     test_parseMessage_2
          , testCase     "parseMessage/3"     test_parseMessage_3

          , testCase     "parse/1"            test_parse_1

          , testCase     "whatWentWrong/1"    test_whatWentWrong_1
          , testCase     "whatWentWrond/2"    test_whatWentWrong_2
          ]
        ]

str1 = "E 2 562 help help"
str2 = "W 22 foo bar"
str3 = "I 56243 xxxyz"
str4 = "Foobar"

msg1 = LogMessage (Error 2) 562 "help help"
msg2 = LogMessage Warning 22 "foo bar"
msg3 = LogMessage Info 56243 "xxxyz"
msg4 = Unknown "Foobar"

test_parseMessage_1 = msg1 @=? parseMessage str1
test_parseMessage_2 = msg2 @=? parseMessage str2
test_parseMessage_3 = msg3 @=? parseMessage str3
test_parseMessage_4 = msg4 @=? parseMessage str4

test_parse_1 = [msg1, msg2, msg3, msg4]
            @=? parse (unlines [str1, str2, str3, str4])

parsedSample = [ LogMessage Info 1 "Nothing to report"
               , LogMessage Warning 2 "Too many pickles"
               , LogMessage (Error 3) 3 "Way too many pickles"
               , LogMessage Info 4 "Everything normal"
               , LogMessage Warning 5 "Flange is due for a check-up"
               , LogMessage Info 6 "Completed armadillo processing"
               , LogMessage Info 7 "Move along, nothing to see here"
               , LogMessage Warning 8 "You really might want to check on that flange"
               , LogMessage Warning 9 "Dude, the FLANGE"
               , LogMessage (Error 99) 10 "Flange failed!"
               , LogMessage Info 11 "Initiating self-destruct sequence"
               ]

correct_1 = [ "Way too many pickles",
              "Bad pickle-flange interaction detected",
              "Flange failed!" ]

test_whatWentWrong_1 = assert $ do
  s <- readFile "sample.log"
  return $ whatWentWrong (parse s) == correct_1

{-
    [ "Flange is due for a check-up"
    , "You really might want to check on that flange"
    , "Dude, the FLANGE"
    ]
-}

correct_2 = [ "Mustardwatch opened, please close for proper functioning!",
              "All backup mustardwatches are busy",
              "Depletion of mustard stores detected!",
              "Hard drive failure: insufficient mustard",
              "All backup mustardwatches are busy",
              "Twenty seconds remaining until out-of-mustard condition",
              "Ten seconds remaining until out-of-mustard condition",
              "Empty mustard reservoir! Attempting to recover...",
              "Recovery failed! Initiating shutdown sequence" ]

test_whatWentWrong_2 = assert $ do
  s <- readFile "error.log"
  return $ whatWentWrong (parse s) == correct_2

{-
    [ "Mustardwatch opened, please close for proper functioning!"
    , "Mustardwatch has less than 15% mustard remaining!"
    , "Mustardwatch has less than 5% mustard remaining!!"
    ]
-}

main = defaultMain tests