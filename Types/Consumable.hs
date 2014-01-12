module Types.Consumable(
  Consumable(..)
) where

import Data.Universe

data Consumable = BombBag 
                | RopePile 
                | BombBox
                deriving (Enum, Eq)

instance Universe Consumable where
  universe = enumFrom BombBag

instance Show Consumable where
  show BombBag  = "bomb bag"
  show BombBox  = "bomb box"
  show RopePile = "rope pile"
  