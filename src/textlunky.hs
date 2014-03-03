{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Default
import Control.Concurrent(threadDelay)
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Lens hiding (Level)
import System.IO
import qualified Data.Map as M

import Types
import Game as G
import Random
import TextlunkyParser

makeLenses ''Player
makeLenses ''Room
makeLenses ''Level
makeLenses ''GameState

main = do
  hSetBuffering stdout NoBuffering
  gen <- newStdGen
  let gs = debug .~ True $ evalRand (randGameState gen) gen -- Set debug flag
  when (not $ gs^.debug) initialize
  G.runGame gs

initialize = do
  forM_ "The walls are shifting............\n" $ \c ->
    do putChar c
       threadDelay 100000
  threadDelay 500000
  putStrLn $ 
    concat $ ["You find yourself in some dark, wet mines with 4 ropes and 4 bombs",
              " in your backpack. You must survive. You may want to take a look around.",
              " What do you do?"]
