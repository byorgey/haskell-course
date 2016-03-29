-- Module for building a big log message String out of a corpus of
-- text and a hand-crafted story. The lines of the story are
-- interspersed into the corpus.
module LogGen where
import Control.Applicative
import Control.Monad
import qualified Data.IntSet as Set
import Data.List (sort, intercalate)
import System.Random
import Control.Monad.Random
import System.Random.Shuffle

import Data.MarkovChain -- from markov-chain on Hackage

-- Strip carriage returns from The Alice's Adventures in Wonderland text.
cleanup = filter (not . null) . map (filter (/= '\r'))

-- Insert time stamps into log messages.
stampLines :: [String] -> [String]
stampLines = zipWith applyStamp [0,1..] . map words
    where applyStamp ts ("I":rst)     = unwords $ "I":show ts:rst
          applyStamp ts ("W":rst)     = unwords $ "W":show ts:rst
          applyStamp ts ("E":lvl:rst) = unwords $ "E":lvl:show ts:rst

-- Apply log message headers to the lines from a corpus.
toLog :: [String] -> IO [String]
toLog = mapM $ (`liftM` randomIO) . toMessage
    where toMessage :: String -> Float -> String
          toMessage m r | r < 0.03  = "E " ++ show (round (r * 10000) `mod` 50) ++ " " ++ m
                        | r < 0.1   = "W " ++ m
                        | otherwise = "I " ++ m

-- Read a corpus of text and turn it into a list of log
-- messages. These log messages lack time stamps.
makeLog :: FilePath -> IO [String]
makeLog = readFile >=> toLog . cleanup . lines

-- Read a log file and turn it into a randomized log file using a
-- Markov walk.
makeRandomLog :: Int   -- ^ number of words to discard from the beginning of each line
              -> Int   -- ^ number of output characters wanted
              -> FilePath -> IO [String]
makeRandomLog n c =  readFile
                 >=> markovize c . dropWords n . cleanup . lines
                 >=> toLog
  where dropWords n = map (unwords . drop n . words)

-- Use a log file as input to a Markov chain algorithm to generate a
-- silly random logfile-ish-like thing.
markovize :: Int -> [String] -> IO [String]
markovize c s = getStdGen >>= return . lines . take c . run 2 (unlines s) 0


-- Intersperse the lines of a story into a mass of noise lines derived
-- from a text corpus.
buildStory :: FilePath -> FilePath -> IO String
buildStory corpusFile logFile
  = do noise1 <- makeLog corpusFile
       noise2 <- makeRandomLog 5 150000 logFile
       noise  <- randomMerge noise1 noise2
       inds   <- uniqueInds (length noise) (length story)
       let ls = stampLines $ intersperse inds story noise
       unlines <$> (evalRandIO $ shuffleM ls)

-- Merge two lists randomly, with 1-4 lines of each at a time
randomMerge :: [a] -> [a] -> IO [a]
randomMerge [] l2 = return l2
randomMerge l1 [] = return l1
randomMerge l1 l2 = do
  n <- randomRIO (1,4)
  (take n l1 ++) `fmap` randomMerge l2 (drop n l1)

-- Generate @n@ unique indices less than @modulus@.
uniqueInds modulus n = aux n Set.empty
    where aux 0 s = return . sort $ Set.toList s
          aux n s = do ind <- ((`rem` modulus) . abs) `liftM` randomIO
                       if Set.member ind s
                         then aux n s
                         else aux (n-1) (Set.insert ind s)

story = [ "E 55 Mustardwatch opened, please close for proper functioning!"
        , "E 76 All backup mustardwatches are busy"
        , "E 88 Depletion of mustard stores detected!"
        , "E 91 Hard drive failure: insufficient mustard"
        , "E 50 All backup mustardwatches are busy"
        , "E 99 Twenty seconds remaining until out-of-mustard condition"
        , "E 88 Ten seconds remaining until out-of-mustard condition"
        , "E 83 Empty mustard reservoir! Attempting to recover..."
        , "E 75 Recovery failed! Initiating shutdown sequence"
        ]

-- This function is rather fragile. Each index must be less than the
-- length of the noise list, and there must be the same number of
-- indices as there are story lines.
intersperse :: [Int] -> [String] -> [String] -> [String]
intersperse [] [] noise = noise
intersperse (ind:inds) (line:lines) noise = pre ++ line : rest
    where (pre,post) = splitAt ind noise
          inds' = map (subtract (ind+1)) inds
          rest = intersperse inds' lines post

main = buildStory "11.txt" "kernel.log" >>= writeFile "error2.log"