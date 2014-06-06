{-# LANGUAGE FlexibleInstances #-}
module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n >= 2 = (fib (n - 1)) + (fib (n - 2))
  | otherwise = undefined

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate fn (0,1)
  where fn (x,y) = (y,x+y)

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s
  
streamToList :: Stream a -> [a]
streamToList (Stream a s) = a:streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x s) = Stream (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = ruler' 0
  where ruler' n = interleaveStreams (streamRepeat n) (ruler' (n + 1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Stream n (streamRepeat 0)
  negate (Stream x xs) = Stream (-x) (negate xs)
  (+) (Stream x xs) (Stream y ys) = Stream (x + y) (xs + ys)
  (*) (Stream x xs) ay@(Stream y ys) = Stream (x * y) ((streamMap (x*) ys) + (xs*ay))
  abs = undefined
  signum = undefined

instance Fractional (Stream Integer) where
  (/) ax@(Stream x xs) ay@(Stream y ys) =
    Stream (x `div` y) (streamMap (`div` y) (xs - (ax/ay) * ys))
  fromRational = undefined

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

data Matrix = Matrix Integer Integer
                     Integer Integer
            deriving Show

instance Num Matrix where
  (*) (Matrix x1 x2 x3 x4) (Matrix y1 y2 y3 y4) =
    Matrix ((x1 * y1) + (x2 * y3)) ((x1 * y2) + (x2 * y4))
           ((x3 * y1) + (x4 * y3)) ((x3 * y2) + (x4 * y4))
  (+) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fib' $ (Matrix 1 1 1 0) ^ n
  where fib' (Matrix _ f _ _) = f

