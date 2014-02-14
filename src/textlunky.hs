{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Default
import Control.Concurrent(threadDelay)
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Lens hiding (Level)
import System.IO
import Types
import Game as G
import Random
import TextlunkyParser

makeLenses ''Player
makeLenses ''Room
makeLenses ''Level
makeLenses ''GameState

-- | A simple test room
testRoom :: Room
testRoom =  rType    .~ KaliAltar 
          $ entities .~ [(fromTriple (0, 0, 0), Enemy' Spider )] 
          $ def :: Room

gs :: Room -> GameState
gs troom = player   .~ plr
         $ levelNum .~ 0
         $ level    .~ (rooms .~ [(fromTuple (0, 0), troom)] $ def)
         $ area     .~ Mines
         $ room     .~ (fromTuple (0, 0), troom)
         $ def :: GameState
  where plr = holding .~ ( Just (GroundItem' PotEmpty) ) 
            $ items   .~ [ClimbingGloves] 
            $ def :: Player

main = do
  --initialize
  g  <- newStdGen
  let room = evalRand randMinesRoom g
  G.runGame (gs room)

initialize = do
  hSetBuffering stdout NoBuffering
  forM_ "The walls are shifting............\n" $ \c ->
    do putChar c
       threadDelay 100000
  threadDelay 500000
  putStrLn $ 
    concat $ ["You find yourself in some dark, wet mines with 4 ropes and 4 bombs",
              " in your backpack. You must survive. You may want to take a look around.",
              " What do you do?"]
