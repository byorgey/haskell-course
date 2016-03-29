
data Tree a
  = Empty
  | Node (Tree a) a (Tree a)

{-
treeSize :: Tree a -> Integer

treeSum :: Tree Integer -> Integer

treeDepth :: Tree a -> Integer

flatten :: Tree a -> [a]
-}

-- foldr :: b -> (a -> b -> b) -> [a] -> b

{-
:: b -> (Tree a -> a -> Tree a -> b) -> Tree a -> b
:: b -> (Tree a -> b) -> Tree a -> b
:: b -> (Tree a -> b -> b) -> Tree a -> b
::      (Tree a -> b -> b) -> Tree a -> b
:: b -> (Tree a -> a -> Tree a -> b -> b) -> Tree a -> b
:: b -> (a -> b) -> (b -> b -> b) -> Tree a -> b
:: b -> (a -> b -> b) -> Tree a -> b
-}

t = Node (Node Empty 3 Empty) 4 (Node Empty 6 Empty)

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree e n Empty = e
foldTree e n (Node l x r) = n (foldTree e n l) x (foldTree e n r)

treeSize' :: Tree a -> Integer
treeSize' = foldTree 0 (\x _ z -> 1 + x + z)

data ExprT
  = Lit Integer
  | Add ExprT ExprT
  | Mul ExprT ExprT

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

foldExprT :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
foldExprT l a m (Lit i)   = l i
foldExprT l a m (Add x y) = a (foldExprT l a m x) (foldExprT l a m y)
foldExprT l a m (Mul x y) = m (foldExprT l a m x) (foldExprT l a m y)

eval' = foldExprT id (+) (*)


-- Monoids

class Monoid' m where
  mempty :: m
  mappend :: m -> m -> m    -- also (<>)

-- Must have:
--   mempty <> x = x
--   x <> mempty = x
--   (x <> y) <> z = x <> (y <> z)

-- but NOT necessarily  x <> y = y <> x

-- e.g.

-- Integers with (+) and 0
-- Integers with (*) and 1
-- 2x2 matrices with (*) and identity matrix
-- Lists with (++) and []

instance Monoid' [a] where
  mempty = []
  mappend = (++)

newtype Sum = S Integer
newtype Product = P Integer

instance Monoid Sum where
  (S x) `mappend` (S y) = S (x + y)

-- non-examples:
-- Integers with (/) and 1

instance (Num a, Num b) => Monoid (a,b) where
  (x1,y1) `mappend` (x2,y2) = -- (x1 + x2, y1 + y2)
                               ((x1,y1), (x2,y2))
