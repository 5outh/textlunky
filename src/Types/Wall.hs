module Types.Wall(
  Wall(..)
) where

import Control.Applicative
import Data.Universe
import Data.Default
import Types.Size
import Types.Jewel
import Types.Item
import Types.Consumable

data Wall = NormalWall
          | GoldWall Size
          | JewelWall Jewel
          | ItemWall Item
          | ConsumableWall Consumable
            deriving Eq

instance Show Wall where
  show NormalWall         = "a wall"
  show (GoldWall  s)      = "a wall with a " ++ show s ++ " chunk of gold in it"
  show (JewelWall j)      = "a wall with a " ++ show j ++ " in it"
  show (ItemWall  i)      = "a wall with "   ++ show i ++ " in it"
  show (ConsumableWall c) = "a wall with a " ++ show c ++ " in it"

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