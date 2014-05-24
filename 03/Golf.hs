module Golf where

import Data.Char
import Data.List

skips :: [a] -> [[a]]
skips xs = map (skip' xs) [0..range]
  where range = (length xs) - 1
        skip' xs n = skip'' (drop n xs) n
        skip'' [] n = []
        skip'' (x:xs) n = x:(skip'' (drop n xs) n)

localMaxima :: [Integer] -> [Integer]
localMaxima = middles . fils . zips
  where zips xs = zip3 xs (drop 1 xs) (drop 2 xs)
        fils = filter (\(a,b,c)->(b > a && b > c))
        middles = map (\(_,x,_)->x)

histogram :: [Integer] -> String
histogram inputs = stringify buckets
  where range = [0..9]
        buckets = ((map ((-1 +) . length)) . group . sort) (range ++ inputs)
        starify = map (\x->if (x > 0) then '*' else ' ')
        stars xs
          | any (>0) xs = (starify xs):(stars (map (-1 +) xs))
          | otherwise = []
        footer = [(replicate 10 '='), (map (intToDigit . fromIntegral) range)]
        stringify bs = intercalate "\n" $ (reverse (stars buckets)) ++  footer
          
