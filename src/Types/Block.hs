module Types.Block(
  Block(..)
) where

import Control.Applicative
import Control.Monad(join)
import Data.Universe
import Types.Size
import Types.Item
import Types.Jewel
import Types.Consumable

data Block =   Dirt
             | CrushBlock
             | Spikes
             | PowderKeg
             | Web 
             | Exit
             | GoldDirt Size -- # gold
             | JewelDirt Jewel -- Jewel type
             | ItemBlock Item
             | ConsumableBlock Consumable
             | ArrowTrap Bool -- fired
             deriving Eq

instance Show Block where
  show Dirt                = "some dirt"
  show (GoldDirt s)        = "a " ++ show s ++ " amount of gold in some dirt"
  show CrushBlock          = "a crushing block"
  show Spikes              = "some spikes"
  show (JewelDirt j)       = "a "  ++ show j ++ " in some dirt"
  show (ItemBlock i)       = show i ++ " in some dirt"
  show (ConsumableBlock c) = "a "  ++ show c  ++ " in some dirt"
  show (ArrowTrap f)       = "an arrowtrap"
  show PowderKeg           = "a powderkeg"
  show Web                 = "a spider web"
  show Exit                = "the exit"
  
instance Universe Block where
  universe = (consts++) . join $ 
                [GoldDirt        <$> sizes,
                 JewelDirt       <$> js,
                 ItemBlock       <$> items,
                 ConsumableBlock <$> cs,
                 ArrowTrap       <$> bs]
   where consts = [Dirt, CrushBlock, Spikes, PowderKeg, Web, Exit, ArrowTrap True]
         sizes = universe :: [Size]
         js    = universe :: [Jewel]
         items = universe :: [Item]
         cs    = universe :: [Consumable]
         bs    = [True, False]