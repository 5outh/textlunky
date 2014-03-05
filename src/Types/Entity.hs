module Types.Entity(
  Entity(..),
  randMinesEntity,
  randMinesBottomEntity,
  randMinesTopEntity,
  randJewel',
  randItem',
  randMinesGroundItem',
  randMinesBlock',
  randMinesEnemy',
  randMinesTopEnemy',
  randMinesBottomEnemy',
  canPickUp
) where

import Data.List(intercalate)
import Data.Universe
import Data.Default
import Control.Lens
import Control.Monad(liftM)
import Types.Jewel
import Types.Item
import Types.GroundItem
import Types.Block
import Types.Enemy
import Types.Direction
import Random.Probability

data Entity = 
   Jewel' Jewel
 | Item' Item
 | GroundItem' GroundItem
 | Block' Block
 | Enemy' Enemy
 | Empty
 deriving Eq

instance Show Entity where
  show (Jewel' j)      = show j
  show (Item' i )      = show i
  show (GroundItem' g) = show g
  show (Enemy' e)      = show e
  show (Block'      b) = show b
  show Empty           = []

randJewel', randItem', randMinesGroundItem' ,
  randMinesBlock'    , randMinesEnemy',
  randMinesTopEnemy' , randMinesBottomEnemy'
  :: MonadRandom m => m Entity
randJewel'            = liftM Jewel'      randJewel
randItem'             = liftM Item'       randItem
randMinesBlock'       = liftM Block'      randMinesBlock
randMinesEnemy'       = liftM Enemy'      randMinesEnemy
randMinesTopEnemy'    = liftM Enemy'      randMinesTopEnemy
randMinesBottomEnemy' = liftM Enemy'      randMinesBottomEnemy
randMinesGroundItem'  = liftM GroundItem' randMinesGroundItem

randMinesEntity :: MonadRandom m => m Entity
randMinesEntity = uniform 
  [ randJewel'         , randItem'      , 
    randMinesGroundItem', randMinesBlock', 
    randMinesEnemy' ] >>= id

-- | Only spawn things that can show up on the ground
randMinesBottomEntity, randMinesTopEntity  :: MonadRandom m => m Entity
randMinesBottomEntity = uniform
  [ randJewel', randMinesGroundItem', randMinesBlock', randMinesBottomEnemy' ] 
  >>= id

randMinesTopEntity = uniform
  [ randMinesBlock', randMinesTopEnemy' ] 
  >>= id

canPickUp :: Entity -> Bool
canPickUp (Jewel' _)      = True
canPickUp (GroundItem' _) = True
canPickUp (Item' _)       = True
canPickUp (Block' _)      = False
canPickUp (Enemy' _)      = False
canPickUp Empty           = False
