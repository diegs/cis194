module Notes where

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex04 = [10,20,30] >>= addOneOrTwo
