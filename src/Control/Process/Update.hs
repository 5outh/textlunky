
{-# LANGUAGE TemplateHaskell #-}
module Control.Process.Update (
  updateP,
  stepGame
) where

import Types
import Control.Lens hiding (Level)
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Control.Monad.State.Class
import Control.Monad
import Prelude hiding (round)
import Data.List(delete)

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Level
makeLenses ''Room

-- | This is the only way that the game will NOT update
-- | when user command is Invalid
stepGame :: Process
stepGame st _ = do
  liftIO $ putStrLn $ "round: " ++ show (st^.round)
  round += 1

-- | Modify the game based on user command
-- | TODO: implement
updateP :: Process

updateP st (Free (YOLO _)) = do
  a <- randDir
  return ()

-- | Move the player one space in some direction
-- | TODO: Fix "go up" allowance
updateP st (Free (Move dir _)) = do
  v@(Vector3 x y z) <- use $ player.loc
  let loc' = case dir of
        -- will want to actually move rooms...
        U -> setV Z 1 v
        D -> setV Z 0 v
        N -> setV Y (min (y+1) 2) v
        S -> setV Y (max (y-1) 0) v
        E -> setV X (max (x-1) 0) v
        W -> setV X (min (x+1) 2) v
  case loc' of
    Just l  -> player.loc .= l
    Nothing -> 
      error "There was a mistake calculating new position in `updateP` over `Move` instruction"

-- | Move the player to some entity
-- | NB. Want to actually make `e` a Vector3 Int
updateP st (Free (MoveTo e _)) = return ()

-- | Pick up the thing that at the player's feet if it can be held
updateP st (Free (Pickup Nothing _)) = do
  loc  <- use $ player.loc
  ents <- use $ room._2.entities -- stuff in the current room
  case lookup loc ents of
    Just x  -> do
      dropItem                              -- drop current item
      room._2.entities %= (delete (loc, x)) -- remove new item from room
      player.holding .= (Just x)            -- put item in hands
    Nothing -> return ()

-- | Drop whatever the player is currently holding
updateP st (Free (DropItem _)) = dropItem

-- | Bomb the ground at player location
updateP st (Free (Bomb Nothing _)) = placeBombAt =<< use (player.loc)

-- | Bomb some location in the room
-- | NB. Drop bomb on floor if not on the ground,
-- |     Don't use bomb if player attempts to bomb up w/o paste
updateP st (Free (Bomb (Just dir) _)) = do
  (Vector3 x y z) <- use $ player.loc
  itms            <- use $ player.items
  let lvl = 
        if Paste `elem` itms
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
  case dir of
    U -> when (Paste `elem` itms) (placeBombAt loc')
    _ -> placeBombAt loc'

updateP _ _ = return ()

-- It's very important that we can do this kind of thing...
placeBombAt :: Vector3 Int -> Global GameState ()
placeBombAt l = do
  let bomb = (l, GroundItem' newBomb)
  room._2.entities %= (bomb:)
  player.bombs -= 1

dropItem :: Global GameState ()
dropItem =  do
  loc  <- use $ player.loc
  hdg  <- use $ player.holding
  case hdg of 
    Just h -> room._2.entities %= ((loc, h) :)
    _      -> return ()
  player.holding .= Nothing