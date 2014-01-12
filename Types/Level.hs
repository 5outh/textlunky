module Types.Level(
  Level(..),
  LevelType(..),
  levelMessage
) where

import Data.Universe
import Data.Default
import Types.Direction
import Types.Room

data LevelType = NormalLevel
               | Dark
               | SkinCrawling
               | SnakePit
               | ChestAndKey
                 deriving (Enum, Eq)
                 
data Level = Level{
  _rooms :: [(Space, Room)],
  _lType :: LevelType
} 

instance Universe LevelType where
  universe = enumFrom NormalLevel

-- NB. Shows message upon entrance
levelMessage :: LevelType -> String
levelMessage t = case t of
  Dark         -> "I can't see a thing!"
  SkinCrawling -> "My skin is crawling!"
  SnakePit     -> "I hear snakes. I hate snakes!"
  _            -> []