
data Maybe' a = Nothing' | Just' a
  deriving Show

instance Monad Maybe' where
  return = Just'
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (Just' x) >>= f = f x
  Nothing'  >>= _ = Nothing'

--  ma >>= f = join (fmap f ma)  -- join :: Maybe (Maybe b) -> Maybe b

halve :: Int -> Maybe' Int
halve x | even x = Just' (x `div` 2)
        | otherwise = Nothing'

check :: Int -> Maybe' Int
check x | x > 100 = Just' x
        | otherwise = Nothing'

{-
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
-}

--   (>>=) = flip concatMap

-- concatMap f = concat . map f



newtype Parser a
  = ParserC { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (ParserC p) = ParserC $ \s -> fmap (first f) (p s)
    where first f (x,y) = (f x, y)

instance Monad Parser where
  return a = ParserC $ \s -> Just (a, s)
  p >>= f  = parserJoin (fmap f p)

parserJoin :: Parser (Parser a) -> Parser a
parserJoin (ParserC p) = ParserC f
  where
    f inp = case p inp of
              Nothing -> Nothing
              Just (ParserC p', inp') -> p' inp'
  -- p :: String -> Maybe (Parser a, String)
  -- p' :: String -> Maybe (a, String)
