
pair :: Applicative f => f a -> f b -> f (a,b)
pair fa fb = (\x y -> (x,y)) <$> fa <*> fb
  --       = (,) <$> fa <*> fb
  --       = liftA2 (,) fa fb
  -- pair  = liftA2 (,)

(*>) :: Applicative f => f a -> f b -> f b

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]

sequenceA :: Applicative f => [f a] -> f [a]

replicateA :: Applicative f => Int -> f a -> f [a]

