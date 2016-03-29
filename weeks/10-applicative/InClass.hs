
type Name = String
type Phone = String

data Employee = Emp Name Phone

nameList  :: [Name]
phoneList :: [Phone]

employeeList :: [Name] -> [Phone] -> [Employee]
-- could zip the lists together
-- or pair in all possible ways

readName :: BigRecord -> Name
readPhone :: BigRecord -> Phone

readEmployee :: (BigRecord -> Name) -> (BigRecord -> Phone) -> (BigRecord -> Employee)
readEmployee n p br = Emp (n br) (p br)

-- f Name -> f Phone -> f Employee

foo :: Functor f => (a -> (b -> c)) -> f a -> f b -> f c
foo g fa fb = magic (fmap g fa) fb
  -- g  :: a -> b -> c
  -- fa :: f a
  -- fb :: f b
  -- fmap g fa :: f (b -> c)

  -- magic :: f (b -> c) -> f b -> f c

class Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b   -- 'app' 'apply' 'splat'
  pure  :: a -> f a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 g fa fb = fmap g fa <*> fb
            -- = g <$> fa <*> fb

liftA3 :: ??? f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 g fa fb fc = (fmap g fa <*> fb) <*> fc
  -- fmap g fa :: f (b -> c -> d)
  
               -- = g <$> fa <*> fb <*> fc



