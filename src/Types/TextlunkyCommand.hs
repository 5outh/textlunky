{-# LANGUAGE DeriveFunctor #-}

module Types.TextlunkyCommand(
  TextlunkyCommand(..)
) where

import Types.Direction
import Types.Enemy
import Types.Entity
import Types.Vectors (Vector3(..))

-- | Derives Functor for Free usage in other areas.
data TextlunkyCommand a = 
    Move   Direction       a
  | MoveTo (Vector3 Int)   a -- | Maybe move to (Vector3 Int)
  | Pickup (Maybe Entity)  a -- | Same ^
  | Jump   (Maybe Enemy )  a -- | Same ^
  | Attack (Maybe Enemy )  a -- | Same ^
  | DropItem               a
  | ShootD Direction       a
  | ShootE Enemy           a -- | Same ^ (shoot enemy in location)
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
  | ShowMe                 a -- | Show Player
  | YOLO                   a -- | Easter egg.
  | End
  {-
  | Explosion (Vector3 Int) a -- | something explodes
  | Movement  (Vector3 Int) Direction a -- | move entity in some direction
  | EntityAttack Entity     a -- | Some entity attacks their location
  | ...etc.
  -}
    deriving (Show, Eq, Functor)

