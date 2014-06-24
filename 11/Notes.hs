module Notes where

import Control.Applicative
import Data.Traversable

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (flip const)

--mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
--mapA f = sequenceA . fmap f

--sequenceA :: Applicative f => [f a] -> f [a]
--sequenceA = 

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n = sequenceA . (replicate n)
