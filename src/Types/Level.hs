{-# LANGUAGE NoMonomorphismRestriction #-}
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

-- TODO: Demolish walls as appropriate to get a path from start room to exit room.
-- need to get a random walk from (x, 2) to (x', 0)
-- Steps, something like:
-- 1. Set start and end nodes
-- 2. Generate a random walk (of Vectors with directions) from start to end (maybe modified)
-- 3. Run through each one, demolishing walls in rooms according to directions
-- 4. Set start and end levels as starting block and ending block
randMinesLevel :: MonadRandom m => m Level
randMinesLevel = do
  let roomLocs = Vector2 <$> [0..2] <*> [0..2] :: [Vector2 Int]
  t      <- randMinesLevelType
  rs     <- replicateM 9 randMinesRoom
  startX <- uniform [0..2]
  let roomsAndLocs = zip roomLocs rs
      startRoomLoc = Vector2 startX 2 -- random room from the top
  return $ Level roomsAndLocs t

randDirForWalk = fromList [(D, 10), (E, 5), (W, 5), (U, 1)]

-- random walk from source to target
randWalk :: MonadRandom m => Vector2 Int -> Vector2 Int -> m [(Direction, Vector2 Int)]
randWalk s@(Vector2 x y) d =
  if s == d then return []
  else do 
    dir <- randDirForWalk
    return []


