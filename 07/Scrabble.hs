{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char
import Data.Monoid

newtype Score = Score Int
              deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score c = Score value
  where scores = [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]
        index = (ord . toLower) c - (ord 'a')
        value | index >= 0 && index < 26 = scores !! index
              | otherwise = 0

scoreString :: String -> Score
scoreString = sum . (map score)

getScore :: Score -> Int
getScore (Score i) = i
