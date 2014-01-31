{-# LANGUAGE TemplateHaskell #-}
module Types.Player(
  Player(..),
  playerSnippet,
  alive
) where

import Data.Default
import Control.Lens
import Data.List(intercalate)
import Types.Direction
import Types.Item
import Types.Entity

data Player = Player{
  _loc   :: Space,
  _hp    :: Int,
  _bombs :: Int,
  _ropes :: Int,
  _gold  :: Int,
  _items :: [Item],         -- | Passive items
  _holding :: Maybe Entity, -- | Current Item in hands
  _favor :: Int             -- | Kali favor
} deriving Eq

-- | Starting Player
instance Default Player where
  def = Player (D,S,E) 4 4 4 0 [] Nothing 0
  
makeLenses ''Player

-- | Full show, exclude favor since it's a hidden stat
instance Show Player where
  show p = intercalate "\n" $ filter (not . null)
    ["You have " ++ show (p^.hp) ++ " hp remaining.",
     "You have " ++ show (p^.bombs)  ++ " bombs remaining.",
     "You have collected " ++ show (p^.gold) ++ " gold so far.",
     if null (p^.items) then [] 
      else "You have collected the following items: " ++ (intercalate ", " $ map show (p^.items)),
     case (p^.holding) of 
      Nothing -> []
      Just a  -> "You are holding : " ++ show (p^.holding)
    ]

-- | Information to show on each round
playerSnippet :: Player -> String
playerSnippet p = 
  "You are in the " 
    ++ (showRelativeDirection (p^.loc)) ++ "."
    ++ case p^.holding of
         Just x -> "\nYou are holding a " ++ show x ++ "."
         _      -> []

----- Extras -----
alive :: Player -> Bool
alive = (<=0) . view hp