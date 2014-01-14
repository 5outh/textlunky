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
    TextlunkyCommand(..),
    UserMoves(..),
    levelMessage,
    srt,
    showRelativeDirection,
    dirs
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
          | IceCaves
          | Temple
          | Hell
            deriving (Show, Eq)

data GameState = GameState{
  _player   :: Player,
  _levelNum :: Int   ,
  _level    :: Level ,
  _area     :: Area  ,
  _room     :: Room  
}
                                  