{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
module Types.Level(
  Level(..),
  LevelType(..),
  randMinesLevel,
  levelMessage,
  randWalk,
  demolishWalls
) where

import Data.Universe
import Data.Default
import Types.Direction
import Types.Room
import Types.Vectors
import Random.Probability
import Control.Applicative
import Control.Monad
import Data.Maybe(fromJust)
import Control.Lens hiding (Level)
import Control.Monad.Trans.State
import qualified Data.Map as M

makeLenses ''Room

data LevelType = NormalLevel
               | Dark
               | SkinCrawling
               | SnakePit
               | ChestAndKey
                 deriving (Enum, Eq)
                 
data Level = Level{
  _rooms :: M.Map (Vector2 Int) Room,
  _start_room :: (Vector2 Int, Room),
  _lType :: LevelType
} deriving Eq

instance Universe LevelType where
  universe = enumFrom NormalLevel

instance Default Level where
  def = Level M.empty undefined NormalLevel

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

randMinesLevel :: MonadRandom m => m Level
randMinesLevel = do
  let roomLocs = Vector2 <$> [0..2] <*> [0..2] :: [Vector2 Int]
  t      <- randMinesLevelType
  rs     <- replicateM 9 randMinesRoom
  startX <- uniform [0..2]
  endX   <- uniform [0..2]
  let roomsAndLocs  = zip roomLocs rs
      startRoomLoc  = Vector2 startX 2 -- random top room
      endRoomLoc    = Vector2 endX   0 -- random bottom room
      startRoom     = fromJust $ lookup startRoomLoc roomsAndLocs -- I only use fromJust here since it is GUARANTEED that such a value exists.
      endRoom       = fromJust $ lookup endRoomLoc   roomsAndLocs
      start         = (startRoomLoc, startRoom)
      setEndRoom (x, r) = if x == endRoomLoc then (x, rType .~ LevelExit $ r) else (x, r)
      setStartRoom (x, r) = if x == startRoomLoc then (x, rType .~ StartRoom $ r) else (x, r)
  walk   <- randWalk startRoomLoc endRoomLoc
  return $ 
    Level
    ( demolishWalls walk (M.fromList $ map (setStartRoom . setEndRoom) roomsAndLocs) )
    start
    t

-- find current and next room, demolish walls in both, add ladders in both if moving u/d.
demolishWalls :: [(Direction, Vector2 Int)] -> M.Map (Vector2 Int) Room -> M.Map (Vector2 Int) Room
demolishWalls []            rms = rms
demolishWalls ((dir, v):ds) rms = demolishWalls ds $ update rms
  where v' = moveVect dir v
        opposite U = D
        opposite D = U
        opposite W = E
        opposite E = W
        opposite _ = error "attempt to move in invalid direction."
        update     = if v' == v then id else M.update (Just . demolish dir) v . M.update (Just . demolish (opposite dir)) v'

randDirForWalk :: (MonadRandom m, Ord a, Show a) => Vector2 a -> Vector2 a -> m Direction
randDirForWalk (Vector2 x y) (Vector2 x' y')
  | x < x'    = fromList [(E, 10), (U, 1), (D, 5)] -- left of
  | x > x'    = fromList [(W, 10), (U, 1), (D, 5)] -- right of
  | y > y'    = fromList [(D, 5), (E, 5), (W, 5), (U, 1)] -- above
  | y < y'    = error $ "y value " ++ show y ++ " is less than minimum y value should be."-- below; should never happen in our scenario
  | otherwise = error "source and target are the same; no more walking should be done at this point."

moveVect :: (Num a, Ord a) => Direction -> Vector2 a -> Vector2 a
moveVect W (Vector2 x y) = Vector2 (max 0 (x-1)) y
moveVect E (Vector2 x y) = Vector2 (min 2 (x+1)) y
moveVect D (Vector2 x y) = Vector2 x (max 0 (y-1))
moveVect U (Vector2 x y) = Vector2 x (min 2 (y+1))
moveVect d _ = error $ "Attempt to move in direction: " ++ show d ++ ". Impossible on a 2d game board!"

-- random walk from source to target
randWalk :: (MonadRandom m, Num a, Show a, Ord a) => 
            Vector2 a  -- source 
         -> Vector2 a  -- target
         -> m [(Direction, Vector2 a)]
randWalk s@(Vector2 x y) t@(Vector2 x' y') =
  if s == t then return []
  else do 
    dir  <- randDirForWalk s t
    next <- randWalk (moveVect dir s) t
    return ((dir, s) : next)
