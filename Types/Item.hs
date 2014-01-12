module Types.Item(
  Item(..) 
)where

import Data.Universe

data Item = ClimbingGloves deriving Eq

instance Show Item where
  show ClimbingGloves = "climbing gloves"

instance Universe Item where
  universe = [ClimbingGloves]
  