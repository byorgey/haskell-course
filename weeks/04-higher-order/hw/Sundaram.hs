import Data.List

sieveSundaram n = map (\x -> 2 * x + 1) . (`sub` ijs) $ [1..n]
    where
        ijs = takeWhile (<= n) . map head . group . sort $ [i + j + 2*i*j | j <- [1..n], i <- [1.. (n - j) `div` (1 + 2*j)]]

        sub [] _  = []
        sub xs [] = xs
        sub xxs@(x:xs) yys@(y:ys)
          | x < y     = x : sub xs yys
          | x == y    = sub xs ys
          | otherwise = sub xxs ys

{-
i + j + 2ij <= n
i(1 + 2j) <= n - j
i <= (n - j)/(1 + 2j)
-}