module Types.Entity(
  Entity(..)
) where

import Data.List(intercalate)
import Data.Universe
import Data.Default
import Control.Lens
import Types.Jewel
import Types.Item
import Types.GroundItem
import Types.Consumable
import Types.Block
import Types.Enemy
import Types.Direction

data Entity = 
   Jewel' Jewel
 | Item' Item
 | GroundItem' GroundItem
 | Consumable' Consumable
 | Block' Block
 | Enemy' Enemy
 | Empty
 deriving Eq

instance Show Entity where
  show (Jewel' j)      = show j
  show (Item' i )      = show i
  show (GroundItem' g) = show g
  show (Enemy' e)      = show e
  show (Consumable' c) = show c
  show (Block'      b) = show b
  show Empty           = []