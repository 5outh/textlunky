module Types.Entity(
  Entity(..),
  randMinesEntity,
  randMinesBottomEntity,
  randMinesTopEntity,
  randJewel',
  randItem',
  randMinesGroundItem',
  randConsumable',
  randMinesBlock',
  randMinesEnemy',
  randMinesTopEnemy',
  randMinesBottomEnemy'
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

randJewel', randItem', randMinesGroundItem' ,
  randConsumable'    , randMinesBlock'      , randMinesEnemy',
  randMinesTopEnemy' , randMinesBottomEnemy'
  :: MonadRandom m => m Entity
randJewel'            = liftM Jewel'      randJewel
randItem'             = liftM Item'       randItem
randConsumable'       = liftM Consumable' randConsumable
randMinesBlock'       = liftM Block'      randMinesBlock
randMinesEnemy'       = liftM Enemy'      randMinesEnemy
randMinesTopEnemy'    = liftM Enemy'      randMinesTopEnemy
randMinesBottomEnemy' = liftM Enemy'      randMinesBottomEnemy
randMinesGroundItem'  = liftM GroundItem' randMinesGroundItem

randMinesEntity :: MonadRandom m => m Entity
randMinesEntity = uniform 
  [ randJewel'     , randItem'      , 
    randMinesGroundItem', randConsumable', randMinesBlock', 
    randMinesEnemy' ] >>= id

-- | Only spawn things that can show up on the ground
randMinesBottomEntity, randMinesTopEntity  :: MonadRandom m => m Entity
randMinesBottomEntity = uniform
  [ randJewel', randMinesGroundItem', randMinesBlock', randMinesBottomEnemy' ] >>= id

randMinesTopEntity = uniform
  [ randMinesBlock', randMinesTopEnemy' ] >>= id