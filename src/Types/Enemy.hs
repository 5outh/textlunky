module Types.Enemy(
  Enemy(..),
  readEnemy,
  randMinesEnemy,
  randMinesTopEnemy,
  randMinesBottomEnemy,
  randPotEnemy
) where

import Data.Universe
import Control.Applicative
import Random.Probability
import Control.AI

-- | Note: Angry Shopkeepers and Boulders are special enemies
-- | Also, these are only the enemies in the Mines
data Enemy =  Snake      { _ehp :: Int, _ai :: AI () }
            | Bat        { _ehp :: Int, _ai :: AI () }
            | Spider     { _ehp :: Int, _ai :: AI () }
            | Cobra      { _ehp :: Int, _ai :: AI () }
            | SpinSpider { _ehp :: Int, _ai :: AI () }
            | Skeleton   { _standing :: Bool, _ehp :: Int, _ai :: AI ()}
            | BigSpider  { _ehp :: Int, _ai :: AI () }
            | Scorpion   { _ehp :: Int, _ai :: AI () }
            | Caveman    { _ehp :: Int, _ai :: AI () }
            | Shopkeeper { _angry   :: Bool, _ehp :: Int, _ai :: AI () }
            | Boulder    { _moving  :: Bool, _ehp :: Int, _ai :: AI () }
            | Arrow      { _resting :: Bool, _ehp :: Int, _ai :: AI () } 
            deriving Eq

mk :: (Int -> AI () -> Enemy) -> Enemy
mk e = e 5 doNothing

instance Show Enemy where
  show (Snake            _ _) = "a green snake" 
  show (Bat              _ _) = "a bat"
  show (Spider           _ _) = "a small spider"
  show (Cobra            _ _) = "a spitting cobra"
  show (SpinSpider       _ _) = "a web-spinning spider"
  show (Skeleton True    _ _) = "a walking skeleton"
  show (Skeleton False   _ _) = "a heap of human bones"
  show (BigSpider        _ _) = "a huge spider" -- 2 dmg
  show (Scorpion         _ _) = "a scorpion"
  show (Caveman          _ _) = "a caveman"
  show (Arrow True       _ _) = "an arrow flying through the air" -- 2 dmg
  show (Arrow False      _ _) = "an arrow resting on the ground"
  show (Shopkeeper False _ _) = "a peaceful shopkeeper"
  show (Shopkeeper True  _ _) = "a very angry shopkeeper" -- stun + 1 dmg
  show (Boulder True     _ _) = "a boulder, rolling quickly" -- 5 dmg
  show (Boulder False    _ _) = "an immobile boulder" -- 0 dmg

readEnemy :: String -> Enemy
readEnemy = mk . readEnemy'
  where 
    readEnemy' "snake" = Snake
    readEnemy' "bat" = Bat
    readEnemy' "spider" = Spider
    readEnemy' "cobra" = Cobra
    readEnemy' "web spider" = SpinSpider
    readEnemy' "web-spinning spider" = SpinSpider
    readEnemy' "walking skeleton" = Skeleton True
    readEnemy' "heap of human bones" = Skeleton False
    readEnemy' "huge spider" = BigSpider
    readEnemy' "scorpion" = Scorpion
    readEnemy' "caveman" = Caveman
    readEnemy' "arrow" = Arrow False -- TODO: idk.
    readEnemy' "shopkeeper" = Shopkeeper False
    readEnemy' "angry shopkeeper" = Shopkeeper True
    readEnemy' "boulder" = Boulder True 
    readEnemy' "immobile boulder" = Boulder False -- TODO: idk.

instance Universe Enemy where
  universe = 
    map mk [Snake, Bat, Spider, Cobra, SpinSpider, BigSpider, Scorpion, Caveman]
    ++ (map mk $ [Skeleton, Shopkeeper, Boulder, Arrow] <*> [True, False])

{- Random Generation -}
-- Any enemy that can randomly spawn in the mines
randMinesEnemy :: MonadRandom m => m Enemy
randMinesEnemy = fromList $ 
  ( withWeight 10 $ map mk 
                   [Snake, Bat, Spider, Cobra, SpinSpider, 
                    Skeleton False, Scorpion, Caveman    ] )
  ++ withWeight 1 [mk BigSpider]

-- Enemies that spawn on the ceiling
randMinesTopEnemy :: MonadRandom m => m Enemy
randMinesTopEnemy = fromList $ 
    withWeight 40 [mk Bat, mk Spider, mk SpinSpider] ++ withWeight 1 [mk BigSpider]

-- Enemies that spawn on the floor
randMinesBottomEnemy :: MonadRandom m => m Enemy
randMinesBottomEnemy = uniform [mk Snake, mk Cobra, mk (Skeleton False), mk Scorpion, mk Caveman]

-- Enemies that spawn in pots
randPotEnemy :: MonadRandom m => m Enemy
randPotEnemy = uniform [mk Snake, mk Cobra, mk Scorpion]
