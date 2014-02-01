module Types.Consumable(
  Consumable(..),
  randConsumable,
  randBombItem
) where

import Data.Universe
import Random.Probability

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

-- 4:1 ratio between bag/pile and boxes
randConsumable :: MonadRandom m => m Consumable
randConsumable = fromList $ 
     withWeight 4 [BombBag, RopePile]
  ++ withWeight 1 [BombBox]

-- Mainly for bomb shops, uniform bomb item
randBombItem :: MonadRandom m => m Consumable
randBombItem = uniform [BombBag, BombBox]