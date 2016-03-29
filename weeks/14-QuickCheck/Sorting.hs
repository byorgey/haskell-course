import           Test.QuickCheck
import           Test.QuickCheck.Function

-- Property-based testing

-- reverse

propReverseLength :: [Int] -> Bool
propReverseLength xs = length xs == length (reverse xs)

propReverseHeadTail :: [Int] -> Property
propReverseHeadTail xs = not (null xs) ==> (head xs == last (reverse xs))

propReverseInverse :: [Int] -> Bool
propReverseInverse xs = xs == reverse (reverse xs)


-- sorting

propSortFirstLast :: ([Int] -> [Int]) -> [Int] -> Property
propSortFirstLast s xs = (not (null xs)) ==> (head (s xs) <= last (s xs))

propSortLength :: ([Int] -> [Int]) -> [Int] -> Bool
propSortLength s xs = length xs == length (s xs)

propSortSorted :: ([Int] -> [Int]) -> [Int] -> Bool
propSortSorted s xs = and $ zipWith (<=) (s xs) (tail (s xs))

propSortIdempotent :: ([Int] -> [Int]) -> [Int] -> Bool
propSortIdempotent s xs = s xs == s (s xs)

sortSuite :: ([Int] -> [Int]) -> IO ()
sortSuite s = do
  quickCheck $ propSortFirstLast s
  quickCheck $ propSortLength s
  quickCheck $ propSortSorted s
  quickCheck $ propSortIdempotent s

foo :: Double -> Double -> Bool
foo x y = x^2 + y^2 < 10

propMapFilter :: Fun Int Int -> Fun Int Bool -> [Int] -> Bool
propMapFilter (Fun _ f) (Fun _ p) xs = map f (filter (p.f) xs) == filter p (map f xs)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs)
  =  quickSort [y | y <- xs, y < x]
  ++ [x]
  ++ quickSort [y | y <- xs, y > x]

----------------------------------------

newtype Mult100 = Mult100 Int
  deriving (Eq, Show)

instance Arbitrary Mult100 where
  arbitrary = fmap (Mult100 . (*100)) arbitrary
