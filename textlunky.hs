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

{- 
  Level: 9 Rooms, one exit, and always a path from top to bottom
  Player commands:
    - look <direction>
    - bomb <direction>
    - rope <direction>
    - pickup <direction> (L/R)
    - get    <item> 
    - both <command> <command>
    - drop <item> (might fail if unattainable)
    - use <direction> (L/R) (item)
    - throw <direction> (L/R/U/D)
    - climb (rope) (wall if climbing gloves)
    - go <direction> (L/R/D typically, U if jetpack)
    - ???
-}

-- a simple test room
testRoom' :: Room
testRoom' = rType    .~ KaliAltar 
          $ entities .~ [( (L, D) , Enemy' Spider ), ( (M, D), Player' p )] 
          $ def :: Room
  where p = holding .~ ( Just (GroundItem' PotEmpty) ) 
          $ items   .~ [ClimbingGloves] 
          $ def :: Player
          
main = undefined