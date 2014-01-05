module Generators(
  randomDir,
  randomDirs
)

where

import qualified System.Random as R
import Types
import Control.Monad.Trans.State
import Control.Monad

randomDir :: State R.StdGen Direction
randomDir = do
  gen <- get
  let dirs = [U, D, L, R, M]
      (idx, gen') = R.randomR (0, 4) gen
      dir  = dirs !! idx
  put gen'
  return dir

randomDirs :: State R.StdGen [Direction]
randomDirs = (sequence . repeat) randomDir

getRandomDir :: R.StdGen -> Direction
getRandomDir = evalState randomDir

getRandomDirs :: R.StdGen -> [Direction]
getRandomDirs = evalState randomDirs