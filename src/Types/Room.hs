{-# LANGUAGE TemplateHaskell #-}
module Types.Room(
  Room(..),
  RoomType(..)
) where

import Data.Default
import Data.Universe
import Data.List(intercalate)
import Types.Direction
import Types.Entity
import Types.Item
import Control.Lens

data RoomType = NormalRoom
              | KaliAltar
              | Shop
              | LevelExit
                deriving (Enum, Eq)

-- | Placeholder
type Wall = Int

data Room = Room{
    _entities :: [(Space, Entity)],
    _rType    :: RoomType,
    -- | See 1/19 notes
    -- | Define like this for absolute correctness,
    -- | Don't allow for an Up wall, for example
    _ladderU  :: Bool,
    _ladderD  :: Bool,
    _wallN    :: Maybe Wall,
    _wallS    :: Maybe Wall,
    _wallE    :: Maybe Wall,
    _wallW    :: Maybe Wall
}

makeLenses ''Room

instance Show Room where
    show r = intercalate "\n" $  (ladders : t : map show' (r^.entities))
        where show' (spc, entity) = case entity of
                _         -> "There is a "  ++ show entity ++ " in the " ++ showRelativeDirection spc ++ "."
              t = case (r^.rType) of
                KaliAltar      -> "You see an altar to Kali."
                Shop           -> "You have found a shop!"
                LevelExit      -> "You have found the exit!"
                _         -> []
              ladders = case (r^.ladderU, r^.ladderD) of
                (True, True) -> "There are ladders both up and down."
                (True, _   ) -> "There is a ladder up."
                (_   , True) -> "There is a ladder down."
                _            -> []
              walls = let ws = map (r^.) [wallN, wallS, wallE, wallW] in
                      map showWall ws
                        -- | TODO: Implement
                        where showWall = undefined


instance Universe RoomType where
  universe = enumFrom NormalRoom
  
--completely void room
instance Default Room where
  def = Room 
          []          -- | Entities
          NormalRoom  -- | Room Type
          False False -- | Ladders
          Nothing Nothing Nothing Nothing -- | Walls

