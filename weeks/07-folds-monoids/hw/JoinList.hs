module JoinList where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- | Yields a new JoinList whose monoidal annotation is derived from those of the two arguments
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

-- | Gets the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- | Finds the JoinList element at the specified index (indexJ i jl) == (jlToList jl !!? i)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ i _ | i < 0    = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Append sz j1 j2)
    | getSize (size sz) <= i      = Nothing         -- Unsure if it should be (... < i) or (... <= i)
    | getSize (size $ tag j1) < i = indexJ (i-1) j2 -- I don't think (i-1) is right - used in definition for (!!?)
    | otherwise                   = indexJ (i-1) j1
indexJ _ _ = Nothing

-- | Drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty       = Empty
dropJ n jl | n <= 0 = jl
-- Incomplete - work out the correct solution to indexJ before proceeding