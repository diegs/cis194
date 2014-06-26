{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                 deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  attackRolls <- replicateM (getAttack b) die
  defenseRolls <- replicateM (getDefense b) die
  return $ updateBattlefield b attackRolls defenseRolls

updateBattlefield :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
updateBattlefield b a d = Battlefield a' d'
  where matchedRolls = zip ((reverse . sort) a) ((reverse . sort) d)
        aWins = length $ filter (uncurry (>)) matchedRolls
        dWins = length matchedRolls - aWins
        a' = attackers b - dWins
        d' = defenders b - aWins

matchRolls :: [DieValue] -> [DieValue] -> [(DieValue, DieValue)]
matchRolls a d = let a' = (reverse . sort) a
                     b' = (reverse . sort) d
                 in zip a' b'

getAttack :: Battlefield -> Int
getAttack b = max 0 $ min 3 $ attackers b - 1

getDefense :: Battlefield -> Int
getDefense b = max 0 $ min 3 $ defenders b

invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
  b' <- battle b
  if terminal b' then return b' else invade b'

terminal :: Battlefield -> Bool
terminal b = defenders b == 0 || attackers b < 2

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  runs <- replicateM 1000 (invade b)
  return $ successRatio runs

successRatio :: [Battlefield] -> Double
successRatio bs = (fromIntegral $ length $ filter success bs) / (fromIntegral $ length bs)
  where success b = (defenders b) == 0

