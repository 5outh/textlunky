module Types.Wall(
  Wall(..),
  innards,
  randWall
) where

import Control.Applicative
import Control.Monad(liftM)
import Data.Universe
import Data.Default
import Types.Size
import Types.Jewel
import Types.Item
import Types.Consumable
import Random.Probability

data Wall = NormalWall
          | GoldWall       Size
          | JewelWall      Jewel
          | ItemWall       Item
          | ConsumableWall Consumable
            deriving Eq

instance Show Wall where
  show NormalWall = "a regular old wall"
  show w          = "a wall with " ++ innards w ++ " in it"

instance Default Wall where
  def = NormalWall

instance Universe Wall where
  universe = (NormalWall :) $ concat 
                [GoldWall        <$> sizes,
                 JewelWall       <$> js   ,
                 ItemWall        <$> items,
                 ConsumableWall  <$> cs   ]
   where sizes = universe :: [Size]
         js    = universe :: [Jewel]
         items = universe :: [Item]
         cs    = universe :: [Consumable]

innards :: Wall -> String
innards NormalWall         = ""
innards (GoldWall  s)      = "a " ++ show s ++ " chunk of gold" 
innards (JewelWall j)      = "a " ++ show j
innards (ItemWall  i)      = show i
innards (ConsumableWall c) = "a " ++ show c

-- mostly normal and gold walls
randWall :: MonadRandom m => m Wall
randWall = fromList [ (return NormalWall,                   30)
                     , (liftM GoldWall       randSize,      10)
                     , (liftM JewelWall      randJewel,      5)
                     , (liftM ItemWall       randItem,       1)
                     , (liftM ConsumableWall randConsumable, 2) ] >>= id
