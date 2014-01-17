module Random.Tools(
  randomFromList',
  randomsFromList',
  randomFromList,
  randomsFromList
) where

import qualified System.Random as R
import Types
import Control.Monad.Trans.State
import Control.Monad
import Data.Universe

-- | Statefully get a random value from a given list of possibilities
randomFromList' :: [a] -> State R.StdGen a
randomFromList' xs = do
  gen <- get
  let (idx, gen') = R.randomR (0, (length xs)) gen
      x = xs !! idx
  put gen'
  return x

-- | Statefully get an infinite list of random values from a list of possibilities
randomsFromList' :: [a] -> State R.StdGen [a]
randomsFromList' = (sequence . repeat) . randomFromList'

-- | Get a random value from a list of possibilities
randomFromList :: R.StdGen -> [a] -> a
randomFromList gen xs = evalState (randomFromList' xs) gen

-- | Get an infinite stream of random values from a list of possibilities
randomsFromList :: R.StdGen -> [a] -> [a]
randomsFromList gen xs = evalState (randomsFromList' xs) gen

-- | Get randoms based on some finite universe
getRandom :: (Finite u) => R.StdGen -> u
getRandom = flip randomFromList universeF

getRandoms :: (Finite u) => R.StdGen -> [u]
getRandoms = flip randomsFromList universeF