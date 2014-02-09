module Types.Level(
  Level(..),
  LevelType(..),
  randMinesLevel,
  levelMessage
) where

import Data.Universe
import Data.Default
import Types.Direction
import Types.Room
import Types.Vectors
import Random.Probability
import Control.Applicative
import Control.Monad

data LevelType = NormalLevel
               | Dark
               | SkinCrawling
               | SnakePit
               | ChestAndKey
                 deriving (Enum, Eq)
                 
data Level = Level{
  _rooms :: [(Vector2 Int, Room)],
  _lType :: LevelType
} 

instance Universe LevelType where
  universe = enumFrom NormalLevel

instance Default Level where
  def = Level [] NormalLevel

-- NB. Shows message upon entrance
levelMessage :: LevelType -> String
levelMessage t = case t of
  Dark         -> "I can't see a thing!"
  SkinCrawling -> "My skin is crawling!"
  SnakePit     -> "I hear snakes. I hate snakes!"
  _            -> []

-- TODO: Keep track of whether or not Chest and Key level has been encountered yet
randMinesLevelType :: MonadRandom m => m LevelType
randMinesLevelType = fromList $ 
      withWeight 5 [NormalLevel]
  ++  withWeight 1 [Dark, SkinCrawling, SnakePit, ChestAndKey] 

--TODO: Demolsih walls as appropriate to get a path from start room to exit room.
randMinesLevel :: MonadRandom m => m Level
randMinesLevel = do
  let roomLocs = Vector2 <$> [0..2] <*> [0..2] :: [Vector2 Int]
  t  <- randMinesLevelType
  rs <- replicateM 9 randMinesRoom
  return $ Level (zip roomLocs rs) t