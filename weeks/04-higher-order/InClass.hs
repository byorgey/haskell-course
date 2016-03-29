
greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter (\x -> x > 100) xs

greaterThan100' xs = filter (>100) xs

{- (? x) = \y -> y ? x
   (x ?) = \y -> x ? y
   -}

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \a -> f (g a)

foo' f g a = f (g a)

myTest xs = even (length (greaterThan100 xs))
myTest' xs = (even . length . greaterThan100) xs
myTest'' = even . length . greaterThan100

bar :: Integer -> (Integer -> Integer)
bar x y = 2*x + y

-- \x y z -> ...  === \x -> \y -> \z -> ...
-- f x y = ...    === f = \x -> \y -> ...

-----------------------------------------------

sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs

product :: [Integer] -> Integer
product [] = 1
product (x:xs) = x * product xs

length :: [a] -> Integer
length [] = 0
length (_:xs) = 1 + length xs

foldr :: r -> (a -> r -> r) -> [a] -> r