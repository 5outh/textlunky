import WeightedQuadTree

import Control.Monad
import Data.List(sort)
import Data.Default
import Types
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
  - climb (rope)
  - go <direction> (L/R/D typically, U if jetpack)
  - ???
-}
------ Extra stuff ---------------------

alive :: Player -> Bool
alive = (<=0) . hp

-- a simple test room
testRoom' :: Room
testRoom' = def{
  entities = [( (L, D) , Enemy' Spider ), ( (M, D), Player' def{ holding = Just (GroundItem' (Pot Empty)), items = [ClimbingGloves]})],
  isKaliAltar' = True
}
---------------------------------------