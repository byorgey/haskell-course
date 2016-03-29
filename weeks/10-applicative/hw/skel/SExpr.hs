{- CIS 194 HW 10
   due Thursday, 29 March
-}

module SExpr where

import Control.Applicative

import AParser

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

------------------------------------------------------------
--  Exercise #6: Parsing S-expressions
------------------------------------------------------------

