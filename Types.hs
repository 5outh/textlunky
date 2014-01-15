{-# LANGUAGE TemplateHaskell #-}
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
    LevelType(..),
    GameState(..),
    Area(..),
    TextlunkyCommand(..),
    levelMessage,
    showRelativeDirection,
    dirs,
    -- | TESTING
    showGS
) where

import Data.Default
import Data.List(intercalate, sort)
import Data.Maybe(isJust)
import Control.Monad(join, replicateM)
import Control.Applicative
import Data.Universe
import Control.Lens hiding (Level, universe)

import Types.Direction
import Types.Size
import Types.Jewel
import Types.Item
import Types.Consumable
import Types.Block
import Types.Enemy
import Types.GroundItem
import Types.Entity
import Types.Room
import Types.Level
import Types.TextlunkyCommand

data Area = Mines
          | Jungle
          | BlackMarket
          | JungleWorm
          | HauntedCastle
          | IceCaves
          | IceCavesWorm
          | Mothership
          | Temple
          | CityOfGold
          | OlmecsLair
          | Hell
            deriving (Show, Eq)

data GameState = GameState{
  _levelNum :: Int   ,
  _level    :: Level ,
  _area     :: Area  ,
  _room     :: Room  
}

instance Default GameState where
  def = GameState 0 undefined Mines undefined
            
makeLenses ''GameState
            
-- | For testing only
showGS = show . view room