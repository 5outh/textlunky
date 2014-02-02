module Types.GroundItem(
  GroundItem(..),
  randChest,
  randPot,
  randCrate,
  randMinesGroundItem
) where

import Control.Applicative
import Control.Monad(join, replicateM, liftM)
import Data.Universe
import Types.Jewel
import Types.Enemy
import Types.Item
import Types.Consumable
import Random.Probability

data GroundItem =  Key
                 | GoldChest
                 | Damsel
                 | Idol
                 | PotEmpty
                 | PotJewel Jewel
                 | PotEnemy Enemy
                 | ConsumableCrate Consumable
                 | ItemCrate Item
                 | Chest [Jewel] 
                 | Floor Consumable
                 deriving Eq

instance Show GroundItem where
  show (PotEmpty)           = "pot"
  show (PotJewel _)         = "pot"
  show (PotEnemy _)         = "pot"
  show (ConsumableCrate _ ) = "crate"
  show (ItemCrate _)        = "crate"
  show (Chest _)            = "chest"
  show Key                  = "key on the ground" -- | Special
  show GoldChest            = "gold chest"        -- | Special
  show Damsel               = "damsel"            -- | One/Level, gen separately
  show Idol                 = "golden idol head"  -- | Special
  show (Floor c)            = show c
                 
instance Universe GroundItem where
  universe =  ([Key, GoldChest, Damsel, Idol, PotEmpty]++) . join $ 
                  [ PotJewel        <$> js,
                    PotEnemy        <$> es,
                    ItemCrate       <$> items,
                    ConsumableCrate <$> cs,
                    Chest           <$> jewelLists,
                    Floor           <$> cs
                  ]
    where js    = universe :: [Jewel]
          es    = universe :: [Enemy]
          items = universe :: [Item]
          cs    = universe :: [Consumable]
          --NB. 1, 2, or 3 jewels per chest; each can be small or large (except diamonds).
          jewelLists = join [ [ [a], [a,b], [a,b,c] ] | (a, b, c) <- zip3 js js js]

randPotEnemy', randPotJewel, randConsumableCrate, 
  randItemCrate, randFloorConsumable :: MonadRandom m => m GroundItem
randPotEnemy'       = liftM PotEnemy        randPotEnemy
randPotJewel        = liftM PotJewel        randLargeJewel
randConsumableCrate = liftM ConsumableCrate randConsumable
randItemCrate       = liftM ItemCrate       randItem
randFloorConsumable = liftM Floor           randConsumable

randChest :: MonadRandom m => m GroundItem
randChest = liftM Chest (replicateM 3 randJewel)

randPot :: MonadRandom m => m GroundItem
randPot = 
  uniform [randPotJewel, randPotEnemy', return PotEmpty] >>= id

randCrate :: MonadRandom m => m GroundItem
randCrate = 
  fromList [(randConsumableCrate, 5), (randItemCrate, 1)] >>= id

randMinesGroundItem :: MonadRandom m => m GroundItem
randMinesGroundItem = 
  fromList [(randChest, 2), (randCrate, 1), (randPot, 5)] >>= id
