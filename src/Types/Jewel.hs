module Types.Jewel(
  Jewel(..),
  randJewel,
  randLargeJewel,
  randSmallJewel,
  mostlySmall,
  mostlyLarge
)

where

import Data.Universe
import Control.Applicative

import Types.Size
import Random.Probability

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

largeJewels = Diamond : ([Sapphire, Ruby, Emerald] <*> pure Large)
smallJewels =            [Sapphire, Ruby, Emerald] <*> pure Small

randJewel :: (MonadRandom m) => m Jewel
randJewel = fromUniverse

randLargeJewel :: (MonadRandom m) => m Jewel
randLargeJewel = uniform largeJewels

randSmallJewel :: (MonadRandom m) => m Jewel
randSmallJewel = uniform smallJewels

-- | For chests
mostlySmall :: (MonadRandom m) => m Jewel
mostlySmall = fromList . concat $ zipWith withWeight [3, 1] [smallJewels, largeJewels]

-- | For completeness, I guess
mostlyLarge :: (MonadRandom m) => m Jewel
mostlyLarge = fromList . concat $ zipWith withWeight [1, 3] [smallJewels, largeJewels]
