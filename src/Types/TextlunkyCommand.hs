{-# LANGUAGE DeriveFunctor #-}

module Types.TextlunkyCommand(
  TextlunkyCommand(..)
) where

import Types.Direction
import Types.Enemy
import Types.Entity

-- | Derives Functor for Free usage in other areas.
data TextlunkyCommand a = 
    Move   Direction       a
  | MoveTo Entity          a
  | Pickup (Maybe Entity)  a
  | Jump   (Maybe Enemy )  a
  | Attack (Maybe Enemy )  a
  | DropItem               a
  | ShootD Direction       a
  | ShootE Enemy           a
  | ShootSelf              a
  | Throw Direction        a
  | Rope                   a
  | Bomb (Maybe Direction) a
  | OpenGoldChest          a
  | OpenChest              a
  | ExitLevel              a
  | DropDown               a
  | Look Direction         a
  | Walls                  a -- | Show walls
  | ShowFull               a -- | Show full room
  | ShowEntities           a -- | Show contents of room
  | End
    deriving (Show, Eq, Functor)

