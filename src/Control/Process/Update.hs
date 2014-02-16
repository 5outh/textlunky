
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
  -- place a bomb at player location
  room._2.entities %= (bomb:)
  player.bombs -= 1

updateP st (Free (Bomb (Just dir) _)) = do
  let (Vector3 x y z) = st^.player^.loc
      lvl = 
        if Paste `elem` (st^.player^.items)
        then z
        else 0
      loc' = case dir of
        U -> Vector3 x y 1
        D -> Vector3 x y 0
        N -> Vector3 x 2 lvl
        S -> Vector3 x 0 lvl
        E -> Vector3 0 y lvl
        W -> Vector3 2 y lvl
        M -> Vector3 1 1 lvl
      bomb = (loc', GroundItem' $ GBomb 2)
  case dir of
    U -> case Paste `elem` (st^.player^.items) of
          True -> do
            room._2.entities %= (bomb:)
            player.bombs -= 1
          _    -> return ()
    _ -> do
      room._2.entities %= (bomb:)
      player.bombs -= 1

updateP _ _ = return ()