{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List(sort)
import Data.Default
import Control.Lens hiding (Level)
import Types
import Generators

makeLenses ''Player
makeLenses ''Room
makeLenses ''Level
makeLenses ''GameState

-- a simple test room
testRoom' :: Room
testRoom' = rType    .~ KaliAltar 
          $ entities .~ [( (L, D) , Enemy' Spider ), ( (M, D), Player' p )] 
          $ def :: Room
  where p = holding .~ ( Just (GroundItem' PotEmpty) ) 
          $ items   .~ [ClimbingGloves] 
          $ def :: Player
          
main = undefined