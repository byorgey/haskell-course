{-
map :: (a -> b) -> [a] -> [b]

mapTree :: (a -> b) -> Tree a -> Tree b

eval :: Exp -> Integer
parse :: String -> Maybe Exp

maybeEval :: Maybe Exp -> Maybe Integer
mapExp :: (Exp -> Integer) -> (Maybe Exp -> Maybe Integer)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b

-- GOAL:
genericMap :: (a -> b) -> t a -> t b
-}

---------
-- Kinds
---------

-- Every value has a type
-- Every type has a kind

-- All "concrete types" (types which classify values)
-- have kind *.

-- Funny :: (* -> *) -> * -> *
data Funny f a = FunnyCons a (f a)

--------

genericMap :: (a -> b) -> t a -> t b
genericMap f = error "not possible"

class OurFunctor f where
  ourFmap :: (a -> b) -> f a -> f b

-- instance OurFunctor Int where  -- kind error
--   ourFmap = undefined

instance OurFunctor [] where
  ourFmap = map

instance OurFunctor Maybe where
  ourFmap g Nothing  = Nothing
  ourFmap g (Just x) = Just (g x)

instance OurFunctor IO where
  -- fmap :: (a -> b) -> IO a -> IO b

  -- (>>=)      :: IO a -> (a -> IO b) -> IO b
  -- flip (>>=) :: (a -> IO b) -> IO a -> IO b
  -- return     :: a -> IO a

  ourFmap f ioa = ioa >>= (return . f)

instance OurFunctor ((->) e) where
  -- ourFmap :: (a -> b) -> (e -> a) -> (e -> b)
  ourFmap = (.)
