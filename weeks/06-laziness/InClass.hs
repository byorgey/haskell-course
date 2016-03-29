

-- Laziness

f x y = x + 2

--  f (release_monkeys(), increment_counter())

-- Laziness ==> purity

f1 :: Maybe a -> [Maybe a]
f1 m = [m , m] -- (m : (m : []))

f2 :: Maybe a -> [a]
f2 Nothing = []
f2 (Just x) = [x]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

{-
  {length (f1 (safeHead [3^500, 49]))}
=
  length {(f1 (safeHead [3^500, 49]))}
=
  {length [safeHead [3^500, 49], safeHead [3^500, 49]]}
=
  2
-}

