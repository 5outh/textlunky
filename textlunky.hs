{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List(sort)
import Data.Default
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Lens hiding (Level)
import Types
import Generators
import Game

makeLenses ''Player
makeLenses ''Room
makeLenses ''Level
makeLenses ''GameState

-- | A simple test room
testRoom :: Room
testRoom =  rType    .~ KaliAltar 
          $ entities .~ [( (U, N, E) , Enemy' Spider ), ( (D, N, E), Player' p )] 
          $ def :: Room
  where p = holding .~ ( Just (GroundItem' PotEmpty) ) 
          $ items   .~ [ClimbingGloves] 
          $ def :: Player
          
main = do
  -- | This will be generated by a seed
  let g = undefined :: GameState
  -- | Discard the result of this computation, because it doesn't matter.
  -- | All work is done in (Game.hs), see that file for notes.
  runFreeT $ evalStateT game g -- IO (Something Complicated)
