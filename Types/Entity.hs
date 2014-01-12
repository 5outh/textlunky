{-# LANGUAGE TemplateHaskell #-}
module Types.Entity(
  Entity(..),
  Player(..),
  alive
) where

import Data.List(intercalate)
import Data.Universe
import Data.Default
import Control.Lens
import Types.Jewel
import Types.Item
import Types.GroundItem
import Types.Consumable
import Types.Block
import Types.Enemy

data Entity = 
   Jewel' Jewel
 | Item' Item
 | GroundItem' GroundItem
 | Consumable' Consumable
 | Block' Block
 | Enemy' Enemy
 | Player' Player
 | Empty
 deriving Eq
 
data Player = Player{
  _hp :: Int,
  _bombs :: Int,
  _ropes :: Int,
  _gold  :: Int,
  _items :: [Item],
  _holding :: Maybe Entity, --current item
  _favor :: Int -- kali favor
} deriving Eq

makeLenses ''Player

-- starting player
instance Default Player where
  def = Player 4 4 4 0 [] Nothing 0

instance Show Entity where
  show (Jewel' j)      = show j
  show (Item' i )      = show i
  show (GroundItem' g) = show g
  show (Enemy' e)      = show e
  show (Consumable' c) = show c
  show (Block'      b) = show b
  show (Player'     p) = show p
  show Empty           = []
  
-- NB. Full show, exclude favor since it's a hidden stat
instance Show Player where
  show (Player health bmbs rps gld itms hdg _) = intercalate "\n" $ filter (not . null)
    ["You have " ++ show health ++ " hp remaining.",
     "You have " ++ show bmbs  ++ " bombs remaining.",
     "You have collected " ++ show gld ++ " gold so far.",
     if null itms then [] 
      else "You have collected the following items: " ++ (intercalate ", " $ map show itms),
     case hdg of 
      Nothing -> []
      Just a  -> "You are holding : " ++ show hdg
    ]

    
----- Extras -----
alive :: Player -> Bool
alive = (<=0) . view hp