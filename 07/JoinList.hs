{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.List
import Data.Monoid

import Buffer
import Editor
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- Example functions.
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty = []
-- jlbToList (Single a) = [a]
-- jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

-- Implementation

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl2 = jl2
(+++) jl1 Empty = jl1
(+++) jl1 jl2 = Append joinedM jl1 jl2
  where joinedM = mappend (tag jl1) (tag jl2)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m jl jr) = m

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl | i < 0 || i >= jlSize jl = Nothing
indexJ _ Empty = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ jl1 jl2)
  | i >= jlSize jl1 = indexJ (i - (jlSize jl1)) jl2
  | otherwise = indexJ i jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ n (Append m jl jr) = dropJ n jl +++ (dropJ (n - jlSize jl) jr)
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ n (Append m jl jr) = (takeJ n jl) +++ (takeJ (n - jlSize jl) jr)
takeJ _ jl = jl

jl :: JoinList Size Char
jl = Append (Size 3)
     (Append (Size 1) (Single (Size 1) 'a') Empty)
     (Append (Size 2) (Single (Size 1) 'b') (Single 1 'c'))

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

instance Buffer (JoinList (Score, Size) String) where
  toString = (intercalate "\n") . jlToList
  fromString = (foldr (+++) Empty) . (map mkSingle) . lines
    where mkSingle l = Single (score l , 1) l
          score l = scoreString l
  line = indexJ
  replaceLine n l b = (takeJ n b) +++ (fromString l) +++ (dropJ (n+1) b)
  numLines = getSize . snd . tag
  value = getScore . fst . tag

jlBuffer :: JoinList (Score, Size) String
jlBuffer = fromString  $ unlines
           [ "This buffer is for notes you don't want to save, and for"
           , "evaluation of steam valve coefficients."
           , "To load a different file, type the character L followed"
           , "by the name of the file."
           ]
           
main = runEditor editor $ jlBuffer
