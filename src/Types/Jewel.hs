module Types.Jewel(
  Jewel(..)
)

where

import Data.Universe
import Control.Applicative
import Types.Size

data Jewel =   Ruby     Size
             | Sapphire Size
             | Emerald  Size
             | Diamond --always big
             deriving Eq

instance Show Jewel where
  show (Ruby s)     = show s ++ " ruby"
  show (Sapphire s) = show s ++ " sapphire"
  show (Emerald s)  = show s ++ " emerald"
  show Diamond      = "diamond"
             
instance Universe Jewel where
  universe = (Diamond :) $ [Sapphire, Ruby, Emerald] <*> [Small, Large]

