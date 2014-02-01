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
import Random.Probability

data Block =   Spikes
             | PowderKeg
             | Web 
             | Exit
             | ArrowTrap Bool -- | Arrow Trap fired?
               deriving Eq

instance Show Block where
  show Spikes              = "a spike pit"
  show (ArrowTrap f)       = "an arrowtrap"
  show PowderKeg           = "an exploding block of dynamite"
  show Web                 = "a spider web"
  show Exit                = "the exit"
  
instance Universe Block where
  universe = (consts++) $ ArrowTrap <$> bs
   where consts = [Spikes, PowderKeg, Web, Exit]
         bs     = [True, False]

-- I'm not convinced the other things are necessary.
-- CrushBlocks can be removed probably,
-- Dirt is sort of contained in Walls now,
-- and Exit is special...
-- For now this seems good.
randMinesBlock :: MonadRandom m => m Block
randMinesBlock = uniform [PowderKeg, Web, Spikes, ArrowTrap False]
