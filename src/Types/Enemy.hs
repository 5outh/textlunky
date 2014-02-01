module Types.Enemy(
  Enemy(..),
  randMinesEnemy
) where

import Data.Universe
import Control.Applicative
import Random.Probability

-- | Note: Angry Shopkeepers and Boulders are special enemies
-- | Also, these are only the enemies in the Mines
data Enemy =  Snake
            | Bat
            | Spider
            | Cobra
            | SpinSpider
            | Skeleton Bool  -- | Is the skeleton standing up?
            | BigSpider
            | Scorpion
            | Caveman
            | Shopkeeper Bool -- | Is the shopkeeper angry?
            | Boulder Bool    -- | Is the boulder moving?
            | Arrow Bool      -- | Is the arrow volatile? 
            deriving Eq

instance Show Enemy where
  show Snake              = "green snake" 
  show Bat                = "bat"
  show Spider             = "large spider"
  show Cobra              = "spitting cobra"
  show SpinSpider         = "web-spinning spider"
  show (Skeleton True)    = "walking skeleton"
  show (Skeleton False)   = "heap of human bones"
  show BigSpider          = "huge spider"
  show Scorpion           = "scorpion"
  show Caveman            = "caveman"
  show (Arrow True)       = "arrow flying through the air"
  show (Arrow False)      = "arrow resting on the ground"
  show (Shopkeeper False) = "passive shopkeeper"
  show (Shopkeeper True)  = "angry shopkeeper"
  show (Boulder True)     = "boulder, rolling quickly"
  show (Boulder False)    = "immobile boulder"
  
instance Universe Enemy where
  universe = [Snake, Bat, Spider, Cobra, SpinSpider, BigSpider, Scorpion, Caveman]
         ++ ([Skeleton, Shopkeeper, Boulder, Arrow] <*> [True, False])

{- Random Generation -}
-- Any enemy that can randomly spawn in the mines
randMinesEnemy :: MonadRandom m => m Enemy
randMinesEnemy = fromList $ 
     withWeight 10 [Snake, Bat, Spider, Cobra, SpinSpider, Skeleton False, Scorpion, Caveman]
  ++ withWeight 1 [BigSpider]
