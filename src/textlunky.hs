{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List(sort)
import Data.Default
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Lens hiding (Level)
import Types
import Game as G
import Random

makeLenses ''Player
makeLenses ''Room
makeLenses ''Level
makeLenses ''GameState

-- | A simple test room
testRoom :: Room
testRoom =  rType    .~ KaliAltar 
          $ entities .~ [( (U, N, E) , Enemy' Spider )] 
          $ def :: Room

gs :: Room -> GameState
gs troom =   player   .~ plr
     $ levelNum .~ 0
     $ level    .~ (rooms .~ [((U, N), testRoom)] $ def)
     $ area     .~ Mines
     $ room     .~ troom
     $ def :: GameState
  where plr = holding .~ ( Just (GroundItem' PotEmpty) ) 
            $ items   .~ [ClimbingGloves] 
            $ def :: Player

-- | GameState will eventually be generated by a random seed
main = do
  g    <- newStdGen
  let room = evalRand randMinesRoom g
  G.runGame (gs room)