{-# LANGUAGE DeriveFunctor #-}

module Types.TextlunkyCommand(
  TextlunkyCommand(..)
) where

import Control.Monad.Trans.Free
import Control.Monad.Identity
import Types.Direction
import Types.Enemy
import Types.Entity

-- | Derives Functor for Free usage in other areas.
data TextlunkyCommand a = 
    Move   Direction a
  | MoveTo Entity a
  | Pickup (Maybe Entity)  a
  | DropItem (Maybe Entity) a
  | Jump   (Maybe Enemy) a
  | Attack (Maybe Enemy) a
  | ShootD Direction a
  | ShootE Enemy a
  | ShootSelf a
  | Throw Direction a
  | Rope a
  | Bomb (Maybe Direction) a
  | OpenGoldChest a
  | OpenChest a
  | ExitLevel a
  | DropDown a
  | Look Direction a
  | End
    deriving (Show, Eq, Functor)

