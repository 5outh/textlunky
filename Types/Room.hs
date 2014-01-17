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

data Room = Room{
    _entities :: [(Space, Entity)],
    _rType    :: RoomType
}

instance Show Room where
    show (Room es r_type) = intercalate "\n" $  (extra: map show' es)
        where show' (spc, entity) = case entity of
                _         -> "There is a "  ++ show entity ++ " in the " ++ showRelativeDirection spc ++ "."
              extra = case r_type of
                KaliAltar      -> "You see an altar to Kali."
                Shop           -> "You have found a shop!"
                LevelExit      -> "You have found the exit!"
                _         -> []

instance Universe RoomType where
  universe = enumFrom NormalRoom
  
--completely void room
instance Default Room where
  def = Room [] NormalRoom