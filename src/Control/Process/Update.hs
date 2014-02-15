
{-# LANGUAGE TemplateHaskell #-}
module Control.Process.Update (
  updateP,
  stepGame
) where

import Types
import Control.Lens hiding (Level)
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Prelude hiding (round)

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Level
makeLenses ''Room

-- | This is the only way that the game will NOT update
-- | when user command is INvalid
stepGame :: Process
stepGame st _ = do
  liftIO $ putStrLn $ "round: " ++ show (st^.round)
  round += 1

-- | Modify the game based on user command
-- | TODO: implement
updateP :: Process
updateP st (Free (Bomb Nothing _)) = do
  let p_loc = st^.player^.loc
      bomb  = (p_loc, GroundItem' $ GBomb 2)
  room._2.entities %= (bomb:)
  -- spawn bomb at player location
  player.bombs -= 1
updateP _ _ = return ()