{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module Types.GameState(
  Area(..),
  GameState(..),
  moveRoom,
  -- | For testing
  showGS,
  randGameState,
  newRNG,
  newMinesLevel
) where

import Control.Lens hiding (Level)
import Data.Default
import Types.Player
import Types.Level
import Types.Direction
import Types.Room
import Types.Vectors
import System.Random
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified Data.Map as M
import Random

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
  _player    :: Player,
  _levelNum  :: Int   ,
  _round     :: Int   ,
  _area      :: Area  ,
  _level     :: Level ,
  _room      :: (Vector2 Int, Room),
  _rng       :: StdGen,
  _debug     :: Bool
}

instance Default GameState where
  def = GameState (def :: Player) 1 1 Mines undefined undefined undefined False
            
makeLenses ''GameState
makeLenses ''Level

-- | For use in random generation
moveRoom :: Direction -> GameState -> GameState
moveRoom d gs = 
  let (Vector2 x y, r) = gs^.room
  in flip moveRoomToV gs $ case d of
      U -> Vector2 x (pred y)
      D -> Vector2 x (succ y)
      E -> Vector2 (pred x) y
      W -> Vector2 (succ x) y
      d -> error $ "Cannot move " ++ show d ++ "during moveRoom"

moveRoomToV :: Vector2 Int -> GameState -> GameState
moveRoomToV v gs =  
  let rms = gs^.level^.rooms
  in case M.lookup v rms of
      Just r -> room .~ (v, r) $ gs
      Nothing -> gs

-- | For testing only
showGS = (\(_, r) -> show r) . view room

-- Random starting GameState
randGameState :: (MonadRandom m) => StdGen -> m GameState
randGameState gen = do
  lvl <- randMinesLevel
  return 
    $ room  .~ (lvl^.start_room)
    $ level .~ lvl
    $ rng   .~ gen
    $ def

-- | These will get used during actual game execution
newRNG :: (Monad m, RandomGen g) => StateT g m ()
newRNG = state next >> return ()

-- newMinesLevel :: Monad m => StateT GameState m ()
newMinesLevel = do
  zoom rng newRNG
  gen <- use rng
  let lvl = evalRand randMinesLevel gen
  level .= lvl
  room  .= (lvl^.start_room)
