module Types (
	Direction(..),
	Position(..),
	Level(..),
	Size(..),
	Jewel(..),
	Item(..),
	Consumable(..),
	Block(..),
	Enemy(..),
	Entity(..),
	GroundItem(..),
	Player(..),
	Room(..),
	GameState(..)

) where

--derive `Ord` for sorting later on.
data Direction = L | R | U | D deriving (Ord, Eq)

type Space = (Direction, Direction) 

type Position = (Int, Int) -- (x, y)

data Level = Level [(Room, Position)]

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

--What I want is for Room to actually be nine blocks
data Room = Room{ 
  exits :: [Direction], --encapsulates next rooms
  blocks :: [(Block, Direction)],
  roomGroundItems :: [(GroundItem, Direction)],
  roomItems :: [(Item, Direction)],
  enemies :: [(Enemy, Direction)],
  isKaliAltar :: Bool, -- allow sacrifices to Kali for an item
  isShop :: Bool,
  isExit :: Bool
}

data GameState = GameState{
  player  :: Player,
  level   :: Level ,
  room    :: Room  
}