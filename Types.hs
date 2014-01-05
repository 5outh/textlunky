module Types (
    Direction(..),
    Level(..),
    Size(..),
    Jewel(..),
    Item(..),
    Consumable(..),
    Block(..),
    Space(..),
    Enemy(..),
    Entity(..),
    GroundItem(..),
    Player(..),
    Room(..),
    RoomType(..),
    GameState(..),
    srt,
    levelMessage,
    showRelativeDirection,
    dirs
) where

import Data.Default
import Data.List(intercalate, sort)
import Data.Maybe(isJust)

-- NB. derive `Ord` for sorting later on.
data Direction = U  | D | M | L | R deriving (Bounded, Ord, Eq)

type Space = (Direction, Direction) 

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

data Entity = 
   Jewel' Jewel
 | Item' Item
 | GroundItem' GroundItem
 | Consumable' Consumable
 | Block' Block
 | Enemy' Enemy
 | Player' Player
 | Empty
 deriving Eq

data RoomType = NormalRoom
              | KaliAltar
              | Shop
              | LevelExit
                deriving Eq
                
data LevelType = NormalLevel
              | Dark
              | SkinCrawling
              | SnakePit
              | ChestAndKey
                deriving Eq
 
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
} deriving Eq

data Room = Room{
    entities :: [(Space, Entity)],
    rType    :: RoomType
}

data Level = Level{
  rooms :: [(Space, Room)],
  lType :: LevelType
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
--completely void room
instance Default Room where
  def = Room [] NormalRoom

----------- Show Instances ---------------

instance Show Direction where 
  show U = "up"
  show D = "down"
  show L = "left"
  show R = "right"
  show M = "middle"

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

instance Show Entity where
    show (Jewel' j)      = show j
    show (Item' i )      = show i
    show (GroundItem' g)  = show g
    show (Enemy' e)      = show e
    show (Consumable' c) = show c
    show (Block'      b) = show b
    show (Player'     p) = show p
    show Empty           = []

instance Show Room where
    show (Room es r_type) = intercalate "\n" $  (extra: map show' es)
        where show' (spc, entity) = case entity of
                Player' p -> playerSnippet p spc
                _         -> "There is a "  ++ show entity ++ " in the " ++ showRelativeDirection spc ++ "."
              extra = case r_type of
                KaliAltar      -> "You see an altar to Kali."
                Shop           -> "You have found a shop!"
                LevelExit      -> "You have found the exit!"
                _         -> []
              playerSnippet p spc = "You are in the " ++ (showRelativeDirection spc) ++ "."
                                ++ 
                                case holding p of
                                  Just x -> "\nYou are holding a " ++ show x ++ "."
                                  _      -> []

-- NB. Full show, exclude favor since it's a hidden stat
instance Show Player where
  show (Player health bmbs rps gld itms hdg _) = intercalate "\n" $ filter (not . null)
    ["You have " ++ show health ++ " hp remaining.",
     "You have " ++ show bmbs  ++ " bombs remaining.",
     "You have collected " ++ show gld ++ " gold so far.",
     if null itms then [] 
      else "You have collected the following items: " ++ (intercalate ", " $ map show itms),
     case hdg of 
      Nothing -> []
      Just a  -> "You are holding : " ++ show hdg
    ]

--- Extras ---

-- NB. Shows message upon entrance
levelMessage :: LevelType -> String
levelMessage t = case t of
  Dark         -> "I can't see a thing!"
  SkinCrawling -> "My skin is crawling!"
  SnakePit     -> "I hear snakes. I hate snakes!"
  _            -> []

-- NB. This is used in `showRelativeDirection`
srt :: (Ord a) => (a, a) -> (a, a)
srt (a, b) = let [a', b'] = sort [a, b] in (a', b')

--NB. All block locations
dirs :: [(Direction, Direction)]
dirs = do
  a <- [L, R, M]
  b <- [M, U, D]
  return (a, b)
  
-- NB. We assume U, D will never be a thing, L, R will never be a thing. So x in {U, D, M}, y in {M, L, R} (ordered sets)
-- | Shows the direction of a space in a room (one of nine spaces)
showRelativeDirection :: Space -> String
showRelative (M, M) = "dead center"
showRelativeDirection a = h ++ " " ++ j
  where (x, y) = srt a
        h = case x of 
            U -> "upper"
            D -> "lower"
            M -> "middle"
        j = case y of 
            L -> "left"
            R -> "right"
            M -> "middle"
