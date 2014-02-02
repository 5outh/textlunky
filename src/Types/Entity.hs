module Types.Entity(
  Entity(..),
  randEntity
) where

import Data.List(intercalate)
import Data.Universe
import Data.Default
import Control.Lens
import Control.Monad(liftM)
import Types.Jewel
import Types.Item
import Types.GroundItem
import Types.Consumable
import Types.Block
import Types.Enemy
import Types.Direction
import Random.Probability

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

randJewel', randItem', randMinesGroundItem',
  randConsumable', randMinesBlock', randMinesEnemy'
  :: MonadRandom m => m Entity
randJewel'           = liftM Jewel'      randJewel
randItem'            = liftM Item'       randItem
randConsumable'      = liftM Consumable' randConsumable
randMinesBlock'      = liftM Block'      randMinesBlock
randMinesEnemy'      = liftM Enemy'      randMinesEnemy
randMinesGroundItem' = liftM GroundItem' randMinesGroundItem

randEntity :: MonadRandom m => m Entity
randEntity = uniform 
  [ return Empty        , randJewel'     , randItem'      , 
    randMinesGroundItem', randConsumable', randMinesBlock', 
    randMinesEnemy' ] >>= id