module Types.Wall(
  Wall(..),
  innards,
  randWall
) where

import Control.Applicative
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

-- 'n' for normal
-- 'g' for gold
-- 'j' for jewel
-- 'i' for item
-- 'c' for consumable
-- mostly normal and gold walls
randWall :: MonadRandom m => m Wall
randWall = do
  t <- fromList [('n', 30), ('g', 10), ('j', 5), ('i', 1), ('c', 2)]
  case t of 
    'n' -> return NormalWall
    'g' -> randSize       >>= (return . GoldWall      )
    'j' -> randJewel      >>= (return . JewelWall     )
    'i' -> randItem       >>= (return . ItemWall      )
    'c' -> randConsumable >>= (return . ConsumableWall)
    _   -> error "The impossible happened! Nothing was chosen."
