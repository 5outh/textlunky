module Types.GroundItem(
  GroundItem(..)
) where

import Control.Applicative
import Control.Monad(join)
import Data.Universe
import Types.Jewel
import Types.Enemy
import Types.Item
import Types.Consumable

data GroundItem =  Key
                 | GoldChest
                 | Damsel
                 | Idol
                 | PotEmpty
                 | PotJewel Jewel
                 | PotEnemy Enemy
                 | Crate Item
                 | Chest [Jewel] 
                 | Floor Consumable
                 deriving Eq

instance Show GroundItem where
  show (PotEmpty)   = "pot"
  show (PotJewel _) = "pot"
  show (PotEnemy _) = "pot"
  show (Crate _ )   = "crate"
  show (Chest _)    = "chest"
  show Key          = "key on the ground"
  show GoldChest    = "gold chest"
  show Damsel       = "damsel"
  show Idol         = "golden idol head"
  show (Floor c)    = show c
                 
instance Universe GroundItem where
  universe =  ([Key, GoldChest, Damsel, Idol, PotEmpty]++) . join $ 
                  [ PotJewel <$> js,
                    PotEnemy <$> es,
                    Crate    <$> items,
                    Chest    <$> jewelLists,
                    Floor    <$> cs
                  ]
    where js    = universe :: [Jewel]
          es    = universe :: [Enemy]
          items = universe :: [Item]
          cs    = universe :: [Consumable]
          --NB. 1, 2, or 3 jewels per chest; each can be small or large (except diamonds).
          jewelLists = join [ [ [a], [a,b], [a,b,c] ] | (a, b, c) <- zip3 js js js]