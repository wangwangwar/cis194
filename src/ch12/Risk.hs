{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Risk
    (
    DieValue (..),
    die,
    getStdGen,
    evalRand,
    Battlefield (..),
    dies,
    battleResults,
    battle,
    (<>),
    invade,
    successProb
    ) where

import Control.Monad.Random
import Data.Monoid
import Data.List
import System.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

-- How to use:
-- do
--   g <- getStdGen
--   let r = evalRand die g
--   print r
die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show, Eq)

-- Exercise 2
-- Simulates a single battle (as explained above) between two
-- opposing armies. That is, it should simulate randomly rolling the
-- appropriate number of dice, interpreting the results, and updating
-- the two armies to reflect casualties. You may assume that each player
-- will attack or defend with the maximum number of units they are
-- allowed.
battle :: Battlefield -> Rand StdGen Battlefield
battle b@Battlefield { attackers = a, defenders = d } = do
  let numberOfAttackers = min (a - 1) 3
  let numberOfDefenders = min d 2
  diesOfAttackers <- dies numberOfAttackers
  diesOfDefenders <- dies numberOfDefenders
  return $ b <> battleResults diesOfAttackers diesOfDefenders

-- Returns a list of random dies
dies :: Int -> Rand StdGen [DieValue]
dies n = sequence (replicate n die)

-- Monoid instance
instance Monoid Battlefield where
  mempty = Battlefield { attackers = 0, defenders = 0 }
  Battlefield { attackers = a1, defenders = d1 } `mappend`
    Battlefield { attackers = a2, defenders = d2 } =
    Battlefield { attackers = a1 + a2, defenders = d1 + d2 }

--
battleResults :: [DieValue] -> [DieValue] -> Battlefield
battleResults [] _ = mempty
battleResults _ [] = mempty
battleResults diesOfAttackers diesOfDefenders = change <>
  (battleResults remainDiesOfAttackers remainDiesOfDefenders)
  where
    dieOfAttacker = head $ reverse $ sort diesOfAttackers
    dieOfDefender = head $ reverse $ sort diesOfDefenders
    remainDiesOfAttackers = tail $ reverse $ sort diesOfAttackers
    remainDiesOfDefenders = tail $ reverse $ sort diesOfDefenders
    change
      | dieOfAttacker > dieOfDefender = Battlefield { attackers = 0, defenders = -1 }
      | otherwise = Battlefield { attackers = -1, defenders = 0 }

-- Exercise 3
-- Simulates an entire invasion attempt, that is, repeated calls
-- to battle until there are no defenders remaining, or fewer than two
-- attackers.
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@Battlefield { attackers = a, defenders = d }
  | a < 2 = return bf
  | d <= 0 = return bf
  | otherwise = do
    newBf <- battle bf
    invade newBf

-- Exercise 4
-- Runs invade 1000 times, and uses the results to compute a
-- Double between 0 and 1 representing the estimated probability that
-- the attacking army will completely destroy the defending army.
-- For example, if the defending army is destroyed in 300 of the 1000
-- simulations (but the attacking army is reduced to 1 unit in the other
-- 700), successProb should return 0.3.
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  bfs <- sequence (replicate 1000 (invade bf))
  let prob = computeProb bfs
  return prob

computeProb :: [Battlefield] -> Double
computeProb bfs = (fromIntegral (length $ filter (\b -> defenders b <= 0) bfs) :: Double) / 1000.0
