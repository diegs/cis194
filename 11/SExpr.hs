{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore f = many_v
  where many_v = some_v <|> pure []
        some_v = liftA2 (:) f many_v

oneOrMore :: Parser a -> Parser [a]
oneOrMore f = some_v
  where many_v = some_v <|> pure []
        some_v = liftA2 (:) f many_v

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = liftA2 (:) p1 p2 
  where p1 = satisfy isAlpha
        p2 = zeroOrMore $ satisfy isAlphaNum

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

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

parseSExpr :: Parser SExpr
parseSExpr = sexpr <|> atom
  where sexpr = Comb <$> (leadTrail (char '(')
                          *> (some parseSExpr)
                          <* (leadTrail (char ')')))
        atom = A <$> (leadTrail ((I <$> ident) <|> (N <$> posInt)))

leadTrail :: Parser a -> Parser a
leadTrail p = spaces *> p <* spaces
