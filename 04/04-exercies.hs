module Main where

import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (-2 +) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (1 /=) . iterate fn
  where fn n = if (even n) then (n `div` 2) else (3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where depth Leaf = (-1)
        depth (Node d _ _ _) = d
        insert x Leaf = Node 0 Leaf x Leaf
        insert x (Node d l y r) =
          let ld = depth l
              rd = depth r
              nl = if (ld < rd) then insert x l else l
              nr = if (ld < rd) then r else insert x r
              nd = max (depth nl) (depth nr) + 1
          in Node nd nl y nr

xor :: [Bool] -> Bool
xor = foldr xor' False
  where xor' True True = False
        xor' False False = False
        xor' _ _ = True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f base [] = base
foldr' f base (x:xs) = f x (foldr' f base xs)
  
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

test n = removes
  where f1 (i,j) = i <= j && i + j + 2*i*j <= n
        f2 (i,j) = i + j + 2*i*j
        removes = sort . map f2 . filter f1 $ cartProd [1..n] [1..n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  map (\x->2*x+1) $ [1..n] \\ (filter (<n) . map (\(i,j) -> i + j + 2*i*j)) (cartProd [1..n] [1..n])

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

