module Types.Enemy(
  Enemy(..)
) where

import Data.Universe

data Enemy =  Snake
            | Bat
            | Spider
            | Cobra
            | SpinSpider
            | Skeleton
            | BigSpider
            | Scorpion
            | Caveman
            | Shopkeeper
            | Boulder
            deriving (Enum, Eq)

instance Show Enemy where
  show Snake       = "green snake" 
  show Bat         = "bat"
  show Spider      = "spider"
  show Cobra       = "spitting cobra"
  show SpinSpider  = "web-spinning spider"
  show Skeleton    = "skeleton"
  show BigSpider   = "large spider"
  show Scorpion    = "scorpion"
  show Caveman     = "caveman"
  show Shopkeeper  = "shopkeeper"
  show Boulder     = "boulder"
  
instance Universe Enemy where
  universe = enumFrom Snake
  