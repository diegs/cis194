module Main where

-- Credit card validation
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10):(toDigitsRev (n `div` 10))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ map (uncurry id) pairs
  where fns = cycle [id, (*2)]
        pairs = zip fns (reverse xs)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate n = (checkSum n) `mod` 10 == 0
  where checkSum = sumDigits . doubleEveryOther . toDigits

-- Hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3 
  | n <= 0 = []
  | n == 1 = [(p1, p2)]
  | otherwise = (hanoi (n - 1) p1 p3 p2) ++ (p1, p2):(hanoi (n - 1) p3 p2 p1)

