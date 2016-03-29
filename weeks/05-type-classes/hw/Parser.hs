-- Applicative parser for infix arithmetic expressions without any
-- dependency on hackage. Builds an explicit representation of the
-- syntax tree to fold over using client-supplied semantics.
module Parser (parseExp) where
import           Control.Applicative
import           Control.Arrow
import           Data.Char
import           Data.List           (foldl')
import           Data.Monoid

-- Building block of a computation with some state of type @s@
-- threaded through it, possibly resulting in a value of type @r@
-- along with some updated state.
newtype State s r = State (s -> Maybe (r, s))

-- Expressions
data Expr = Lit Integer
          | Add Expr Expr
          | Mul Expr Expr
            deriving Show

instance Functor (State s) where
    fmap f (State g) = State $ fmap (first f) . g

instance Applicative (State s) where
    pure x = State $ \s -> Just (x, s)
    State f <*> State g = State $ \s ->
                          case f s of
                            Nothing -> Nothing
                            Just (r, s') -> fmap (first r) . g $ s'

instance Alternative (State s) where
    empty = State $ const Nothing
    State f <|> State g = State $ \s -> maybe (g s) Just (f s)

-- A parser threads some 'String' state through a computation that
-- produces some value of type @a@.
type Parser a = State String a

-- Parse one numerical digit.
digit :: Parser Integer
digit = State $ parseDigit
    where parseDigit [] = Nothing
          parseDigit s@(c:cs)
              | isDigit c = Just (fromIntegral $ digitToInt c, cs)
              | otherwise = Nothing

-- Parse an integer. The integer may be prefixed with a negative sign.
num :: Parser Integer
num = maybe id (const negate) <$> optional (char '-') <*> (toInteger <$> some digit)
    where toInteger = foldl' ((+) . (* 10)) 0

-- Parse a single white space character.
space :: Parser ()
space = State $ parseSpace
    where parseSpace [] = Nothing
          parseSpace s@(c:cs)
              | isSpace c = Just ((), cs)
              | otherwise = Nothing

-- Consume zero or more white space characters.
eatSpace :: Parser ()
eatSpace = const () <$> many space

-- Parse a specific character.
char :: Char -> Parser Char
char c = State parseChar
    where parseChar [] = Nothing
          parseChar (x:xs) | x == c = Just (c, xs)
                           | otherwise = Nothing

-- Succeed only if the end of the input has been reached.
eof :: Parser ()
eof = State parseEof
    where parseEof [] = Just ((),[])
          parseEof _  = Nothing

-- Parse an infix arithmetic expression consisting of integers, plus
-- signs, multiplication signs, and parentheses.
parseExpr :: Parser Expr
parseExpr = (buildOp <$> parseTerm <*> optional ((,) <$> op '+' <*> parseExpr)) <* eatSpace

parseTerm = buildOp <$> parseAtom <*> optional ((,) <$> op '*' <*> parseTerm)

parseAtom = eatSpace *> (parens parseExpr <|> Lit <$> num)

buildOp x Nothing        = x
buildOp x (Just (op, y)) = x `op` y

-- Parse one of our two supported operator symbols.
op :: Char -> Parser (Expr -> Expr -> Expr)
op '+' = eatSpace *> (const Add <$> char '+')
op '*' = eatSpace *> (const Mul <$> char '*')

-- Parse something enclosed in parentheses
parens :: Parser a -> Parser a
parens p = eatSpace *> char '(' *> p <* eatSpace <* char ')'

-- Run a parser over a 'String' returning the parsed value and the
-- remaining 'String' data.
execParser :: Parser a -> String -> Maybe (a, String)
execParser (State f) = f

-- Run a parser over a 'String' returning the parsed value.
evalParser :: Parser a -> String -> Maybe a
evalParser = (fmap fst .) . execParser

-- Parse an arithmetic expression using the supplied semantics for
-- integral constants, addition, and multiplication.
parseExp :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> String -> Maybe a
parseExp con add mul = (convert <$>) . evalParser (parseExpr <* eof)
    where convert (Lit x) = con x
          convert (Add x y) = add (convert x) (convert y)
          convert (Mul x y) = mul (convert x) (convert y)
