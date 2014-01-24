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
             | ArrowTrap Bool -- | Arrow Trap fired?
               deriving Eq

instance Show Block where
  show Dirt                = "some dirt"
  show CrushBlock          = "a crushing block"
  show Spikes              = "some spikes"
  show (ArrowTrap f)       = "an arrowtrap"
  show PowderKeg           = "a powderkeg"
  show Web                 = "a spider web"
  show Exit                = "the exit"
  
instance Universe Block where
  universe = (consts++) $ ArrowTrap <$> bs
   where consts = [Dirt, CrushBlock, Spikes, PowderKeg, Web, Exit, ArrowTrap True]
         bs     = [True, False]