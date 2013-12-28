import WeightedQuadTree

import Control.Monad
import Data.Default
import Data.List(intercalate, sort)
import Types
{- 
Level: 9 Rooms, one exit, and always a path from top to bottom
Player commands:
  - look <direction>
  - bomb <direction>
  - rope <direction>
  - pickup <direction> (L/R)
  - get    <item> 
  - both <command> <command>
  - drop <item> (might fail if unattainable)
  - use <direction> (L/R) (item)
  - throw <direction> (L/R/U/D)
  - climb (rope)
  - go <direction> (L/R/D typically, U if jetpack)
  - ???
-}

-------------------- Default instances ---

-- starting player
instance Default Player where
  def = Player 4 4 4 0 [] Nothing 0

-- completely void room
instance Default Room where
  def = Room [] [] [] [] [] False False False

----------- Show Instances ---------------

instance Show Size where
    show Small = "small"
    show Large = "large"

instance Show Jewel where
  show (Ruby s)     = show s ++ " ruby"
  show (Sapphire s) = show s ++ " sapphire"
  show (Emerald s)  = show s ++ " emerald"
  show Diamond      = "diamond"

instance Show Item where
  show ClimbingGloves = "climbing gloves"

instance Show Consumable where
  show BombBag  = "bomb bag"
  show BombBox  = "bomb box"
  show RopePile = "rope pile"

instance Show Block where
  show Dirt                = "some dirt"
  show (GoldDirt s)        = "a " ++ show s ++ " amount of gold in some dirt"
  show CrushBlock          = "a crushing block"
  show Spikes              = "some spikes"
  show (JewelDirt j)       = "a "  ++ show j ++ " in some dirt"
  show (ItemBlock i)       = "an " ++ show i ++ " in some dirt"
  show (ConsubableBlock c) = "a " ++ show c  ++ " in some dirt"
  show (ArrowTrap f)       = "an arrowtrap"
  show PowderKeg           = "a powderkeg"
  show Web                 = "a spider web"
  show Exit                = "the exit"

instance Show Enemy where
  show Snake       = "green snake" 
  show Bat         = "bat"
  show Spider      = "spider"
  show Cobra       = "spitting snake"
  show SpinSpider  = "web-spinning spider"
  show Skeleton    = "skeleton"
  show BigSpider   = "large spider"
  show Scorpion    = "scorpion"
  show Caveman     = "caveman"
  show Shopkeeper  = "shopkeeper"
  show Boulder     = "boulder"

instance Show GroundItem where
  show (Pot _)     = "pot" 
  show (Crate _ )  = "crate"
  show (Chest _)   = "chest"
  show Key         = "key on the ground"
  show GoldChest   = "gold chest"
  show Damsel      = "damsel"
  show Idol        = "golden idol head"
  show (Floor c)   = show c

instance Show Room where 
  show (Room es blks rgis rmis ems kali shop exit) = rm
    where rm = intercalate "\n" $ fmap (intercalate "\n") $ filter (not . null) $
                [
                 map (\dir -> "There is an exit "         ++ show dir) es, 
                 map (\(blk, dir) -> "You see a " ++ show blk    ++ " to your "       ++ show dir) blks,
                 map (\(itm, dir) -> "You see a " ++ show itm    ++ " on the ground " ++ show dir) rgis,
                 map (\(itm, dir) -> "You see a " ++ show itm    ++ " on the ground " ++ show dir) rmis,
                 map (\(emy, dir) -> "You see a " ++ show emy    ++  " "              ++ show dir) ems,
                 if kali then ["You see an altar in the center of the room"]
                    else if shop then ["You have found a shop!"]
                      else if exit then ["You have found the exit!"]
                        else []
                ]

------ Extra stuff ---------------------
-- This will be used for player-relative messages
srt :: (Ord a) => (a, a) -> (a, a)
srt (a, b) = let [a', b'] = sort [a, b] in (a', b')

alive :: Player -> Bool
alive = (<=0) . hp

-- a simple test room
testRoom :: Room
testRoom = def{
  exits = [R, D],
  enemies = [(Spider, U)],
  roomGroundItems = [(Pot Empty, L), (Floor BombBag, R)]
}
---------------------------------------
