

-- f :: a -> a -> a
-- f x y = x + y

{-
g :: a -> a -> a
g x y = x && y


g x y =
  case (typeOf x) of
    Int  -> x + y
    Bool -> x && y
    _    -> x
-}

-- Parametric polymorphism

f :: a -> a -> a
f x y = y

f' :: a -> a -> a
f' x y = x

f'' :: a -> a -> a
f'' x y = -- error "don't call me"
          f'' x y

f1 :: a -> a
f1 x = x

f2 :: a -> b
f2 x = f2 x

f3 :: a -> b -> a
f3 x _ = x

f4 :: [a] -> [a]
f4 x = x

f4' = reverse
f4'' = take 10
f4''' xs = xs ++ xs

f5 :: (b -> c) -> (a -> b) -> (a -> c)
f5 = (.)

f6 :: (a -> a) -> a -> a
f6 f x = f x
f6' _ x = x
f6'' f x = f (f x)
f6''' f = f
f6'''' f = f . f

-- Type classes
{-
class Eq a where
  (==) :: a -> a -> Bool
  x == y = not (x /= y)
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)
-}

data Foo = F Int | G (Char -> Int)
  deriving (Eq)

{-
instance Eq Foo where
  (==) (F x) (F y) = x == y
  (G x) == (G y) = x == y
  _ == _ = False
-}

instance Eq a => Eq [a] where
  (x:xs) == (y:ys) = x == y && xs == ys
  [] == [] = True
  _ == _  = False

data Maybe a =
    Nothing
  | Just a

class Foo a b where
  bar :: a -> b -> Bool
  baz :: a -> a -> a

  read :: String -> a