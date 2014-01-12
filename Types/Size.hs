module Types.Size(
  Size(..)
)

where

import Data.Universe

data Size = Small | Large deriving (Enum, Eq)

instance Show Size where
    show Small = "small"
    show Large = "large"

instance Universe Size where
  universe = [Small, Large]