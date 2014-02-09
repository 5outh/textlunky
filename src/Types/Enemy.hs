module Types.Enemy(
  Enemy(..),
  randMinesEnemy,
  randMinesTopEnemy,
  randMinesBottomEnemy,
  randPotEnemy
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
  show Snake              = "a green snake" 
  show Bat                = "a bat"
  show Spider             = "a large spider"
  show Cobra              = "a spitting cobra"
  show SpinSpider         = "a web-spinning spider"
  show (Skeleton True)    = "a walking skeleton"
  show (Skeleton False)   = "a heap of human bones"
  show BigSpider          = "a huge spider"
  show Scorpion           = "a scorpion"
  show Caveman            = "a caveman"
  show (Arrow True)       = "an arrow flying through the air"
  show (Arrow False)      = "an arrow resting on the ground"
  show (Shopkeeper False) = "a peaceful shopkeeper"
  show (Shopkeeper True)  = "a very angry shopkeeper"
  show (Boulder True)     = "a boulder, rolling quickly"
  show (Boulder False)    = "an immobile boulder"
  
instance Universe Enemy where
  universe = [Snake, Bat, Spider, Cobra, SpinSpider, BigSpider, Scorpion, Caveman]
         ++ ([Skeleton, Shopkeeper, Boulder, Arrow] <*> [True, False])

{- Random Generation -}
-- Any enemy that can randomly spawn in the mines
randMinesEnemy :: MonadRandom m => m Enemy
randMinesEnemy = fromList $ 
     withWeight 10 [Snake, Bat, Spider, Cobra, SpinSpider, Skeleton False, Scorpion, Caveman]
  ++ withWeight 1 [BigSpider]

-- Enemies that spawn on the ceiling
randMinesTopEnemy :: MonadRandom m => m Enemy
randMinesTopEnemy = fromList $ 
    withWeight 40 [Bat, Spider, SpinSpider] ++ withWeight 1 [BigSpider]

-- Enemies that spawn on the floor
randMinesBottomEnemy :: MonadRandom m => m Enemy
randMinesBottomEnemy = uniform [Snake, Cobra, Skeleton False, Scorpion, Caveman]

-- Enemies that spawn in pots
randPotEnemy :: MonadRandom m => m Enemy
randPotEnemy = uniform [Snake, Cobra, Scorpion]
