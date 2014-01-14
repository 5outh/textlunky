module TextlunkyCommand(
) where

import Types.Direction
import Types.Enemy
import Types.Entity

-- NB. Nothing => default behavior defined in notes.md
data TextlunkyCommand = 
    Move   Direction
  | MoveTo Entity
  | Pickup (Maybe Entity)
  | Jump   (Maybe Enemy)
  | Attack (Maybe Enemy)
  | ShootD Direction
  | ShootE Enemy
  | ShootSelf
  | Throw Direction
  | Rope
  | Bomb (Maybe Direction)
  | OpenGoldChest
  | OpenChest
    deriving (Show, Eq)
