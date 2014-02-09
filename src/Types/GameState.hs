{-# LANGUAGE TemplateHaskell #-}
module Types.GameState(
  Area(..),
  GameState(..),
  moveRoom,
  -- | For testing
  showGS
) where

import Control.Lens hiding (Level)
import Data.Default
import Types.Player
import Types.Level
import Types.Direction
import Types.Room
import Types.Vectors

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
  _player   :: Player,
  _levelNum :: Int   ,
  _level    :: Level ,
  _area     :: Area  ,
  _room     :: (Vector2 Int, Room)
}

instance Default GameState where
  def = GameState (def :: Player) 0 undefined Mines undefined
            
makeLenses ''GameState

-- | For use in random generation
moveRoom :: Direction -> GameState -> GameState
moveRoom d = undefined

-- | For testing only
showGS = (\(_, r) -> show r) . view room