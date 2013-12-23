import Control.Monad
import Data.Default

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

--- Types ------------------------------
type Position = (Int, Int) -- (x, y)

data Level = Level [(Room, Position)]

data Dir = U | D | L | R deriving Eq

data Size = Small | Large deriving Eq

data Jewel =   Ruby     Size
             | Sapphire Size
             | Emerald  Size
             | Diamond --always big
             deriving Eq

data Item = ClimbingGloves deriving Eq

data Consumable = BombBag 
                | RopePile 
                | BombBox
                deriving Eq

data Block =   Dirt
             | GoldDirt Size -- # gold
             | CrushBlock
             | Spikes
             | JewelDirt Jewel -- Jewel type
             | ItemBlock Item
             | ConsubableBlock Consumable
             | ArrowTrap Bool -- fired
             | PowderKeg
             | Web 
             | Exit
             deriving Eq

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
            deriving Eq

data Entity = Empty deriving (Show, Eq) --yet to be implemented

data GroundItem =  Pot Entity   --what's in the pot?
                 | Crate Item
                 | Chest [Jewel] 
                 | Key
                 | GoldChest
                 | Damsel
                 | Idol
                 | Floor Consumable
                 deriving Eq
data Player = Player{
  hp :: Int,
  bombs :: Int,
  ropes :: Int,
  gold  :: Int,
  items :: [Item],
  holding :: Maybe Entity, --current item
  favor :: Int -- kali favor
}

data Room = Room{ 
  exits :: [Dir], --encapsulates next rooms
  blocks :: [(Block, Dir)],
  roomGroundItems :: [(GroundItem, Dir)],
  roomItems :: [(Item, Dir)],
  enemies :: [(Enemy, Dir)],
  isKaliAltar :: Bool, -- allow sacrifices to Kali for an item
  isShop :: Bool,
  isExit :: Bool
}

data GameState = GameState{
  player  :: Player,
  level   :: Level ,
  room    :: Room  
}

-------------------- Default instances ---

-- starting player
instance Default Player where
  def = Player 4 4 4 0 [] Nothing 0

-- completely void room
instance Default Room where
  def = Room [] [] [] [] [] False False False

----------- Show Instances ---------------

instance Show Dir where
    show R = "to your right"
    show L = "to your left"
    show U = "above you"
    show D = "below you"

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

-- too lazy right now to make this a show instance, I goofed a little by making this an IO op. first off
write (Room es blks rgis rmis ems kali shop exit) = do
    forM_ es $ \dir ->          putStrLn $ "There is an exit "         ++ show dir
    forM_ blks $ \(blk, dir) -> putStrLn $ "You see a " ++ show blk    ++ " to your " ++ show dir
    forM_ rgis $ \(itm, dir) -> putStrLn $ "You see a " ++ show itm    ++ " on the ground " ++ show dir
    forM_ rmis $ \(itm, dir) -> putStrLn $ "You see a " ++ show itm    ++ " on the ground " ++ show dir
    forM_ ems  $ \(emy, dir) -> putStrLn $ "You see a " ++ show emy    ++  " "              ++ show dir
    if kali then putStrLn "You see an altar in the center of the room" 
      else if shop then putStrLn "You have found a shop!" 
        else if exit then putStrLn "You have found the exit!" 
          else return ()
    return ()

------ Extra stuff ---------------------

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
