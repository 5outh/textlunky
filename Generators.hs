module Generators(
  randomDir',
  randomDirs',
  randomDir,
  randomDirs
)

where

import qualified System.Random as R
import Types
import Control.Monad.Trans.State
import Control.Monad


{- General convenience functions -}

--NB. Get random value from list
randomFromList' :: [a] -> State R.StdGen a
randomFromList' xs = do
  gen <- get
  let (idx, gen') = R.randomR (0, (length xs)) gen
      x = xs !! idx
  put gen'
  return x

--NB. (infinite) list of random values from list
randomsFromList' :: [a] -> State R.StdGen [a]
randomsFromList' = (sequence . repeat) . randomFromList'

--NB. "Just get it for me" generators
randomFromList :: R.StdGen -> [a] -> a
randomFromList gen xs = evalState (randomFromList' xs) gen

randomsFromList :: R.StdGen -> [a] -> [a]
randomsFromList gen xs = evalState (randomsFromList' xs) gen

{-- Direction generators --}

randomDir' :: State R.StdGen Direction
randomDir' = randomFromList' [U, D, L, R, M]

randomDirs' :: State R.StdGen [Direction]
randomDirs' = randomsFromList' [U, D, L, R, M]

randomDir :: R.StdGen -> Direction
randomDir = evalState randomDir'

randomDirs :: R.StdGen -> [Direction]
randomDirs = evalState randomDirs'

{-- Room Generator Tools --}


randomRoom' :: State R.StdGen Room
randomRoom' = undefined