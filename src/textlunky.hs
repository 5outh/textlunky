{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List(sort)
import Data.Default
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Lens hiding (Level)
import Types
import Types.Vectors
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
  g  <- newStdGen
  let room = evalRand randMinesRoom g
  G.runGame (gs room)