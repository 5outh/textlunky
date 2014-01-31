module Types.Size(
  Size(..),
  randSize
)

where

import Data.Universe
import Random.Probability

data Size = Small | Large deriving (Enum, Eq)

instance Show Size where
    show Small = "small"
    show Large = "large"

instance Universe Size where
  universe = [Small, Large]

randSize :: (MonadRandom m) => m Size
randSize = fromUniverse