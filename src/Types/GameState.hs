{-# LANGUAGE TemplateHaskell #-}
module Types.GameState(
  Area(..),
  GameState(..),
  -- | For testing
  showGS
) where

import Control.Lens hiding (Level)
import Data.Default
import Types.Player
import Types.Level
import Types.Room

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
  _room     :: Room  
}

instance Default GameState where
  def = GameState (def :: Player) 0 undefined Mines undefined
            
makeLenses ''GameState
            
-- | For testing only
showGS = show . view room