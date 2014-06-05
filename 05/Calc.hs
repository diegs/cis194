{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
module Calc where

import Control.Monad
import qualified Data.Map as M

import ExprT
import Parser
import qualified StackVM as S

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

evalStr :: String -> Maybe Integer
evalStr = liftM eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

reify :: ExprT -> ExprT
reify = id

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr S.Program where
   lit x = [S.PushI x]
   add x y = x ++ y ++ [S.Add]
   mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add x y = VAdd x y
  mul x y = VMul x y

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = (\m -> (M.lookup x m))

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = (\_ -> Just x)
  add x y = (\m -> liftM2 (+) (x m) (y m))
  mul x y =  (\m -> liftM2 (*) (x m) (y m))

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
