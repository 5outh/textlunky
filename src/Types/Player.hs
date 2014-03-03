{-# LANGUAGE TemplateHaskell #-}
module Types.Player(
  Player(..),
  playerSnippet,
  alive,
  moveToEntity
) where

import Data.Default
import Control.Lens
import Data.List(intercalate)
import Types.Direction
import Types.Consumable
import Types.Item
import Types.Block
import Types.Enemy
import Types.Jewel
import Types.GroundItem
import Types.Entity
import Types.Vectors
import Types.PState

data Player = Player{
  _loc   :: Vector3 Int,
  _hp    :: Int,
  _bombs :: Int,
  _ropes :: Int,
  _gold  :: Int,
  _items :: [Item],         -- | Passive items
  _holding :: Maybe Entity, -- | Current Item in hands
  _favor :: Int,            -- | Kali favor
  _p_state :: PState
} deriving Eq

-- | Starting Player
instance Default Player where
  def = Player (fromTriple (0,0,0)) 4 4 4 0 [] Nothing 0 Standing
  
makeLenses ''Player

-- | Full show, exclude favor since it's a hidden stat
instance Show Player where
  show p = unlines $ filter (not . null)
    ["You are in the " ++ (showRelativeDirection (fromVector3 $ p^.loc)) ++ " of the room.",
     "You have " ++ show (p^.hp) ++ " hp remaining.",
     "You have " ++ show (p^.bombs)  ++ " bombs remaining.",
     "You have " ++ show (p^.ropes) ++ " ropes remaining.",
     "You have collected " ++ show (p^.gold) ++ " gold so far.",
     if null (p^.items) then [] 
      else "You have collected the following items: " ++ (intercalate ", " $ map show (p^.items)),
     case (p^.holding) of 
      Nothing -> []
      Just a  -> "You are holding : " ++ show a
    ]

-- | Information to show on each round
playerSnippet :: Player -> String
playerSnippet p = 
  "You are in the " 
    ++ (showRelativeDirection (fromVector3 $ p^.loc)) ++ "."
    ++ case p^.holding of
         Just x -> "\nYou are holding a " ++ show x ++ "."
         _      -> []

----- Extras -----
alive :: Player -> Bool
alive = (<=0) . view hp

pickupConsumbale :: Consumable -> Player -> Player
pickupConsumbale BombBox  = bombs +~ 12
pickupConsumbale BombBag  = bombs +~ 4
pickupConsumable RopePile = ropes +~ 4

-- What when the player moves onto another an entity?
-- NB. This should happen LAST: player should have the opportunity to
--     whip or whatever on the current spot and move out of the way
--     before this fires.
moveToEntity ::  Vector3 Int -- | Location
        -> Entity            -- | Target 
        -> Player            -- | Source
        -> Player            -- | Result

moveToEntity v (Jewel' j) = (loc .~ v) . (gold +~ value j)

moveToEntity v (Block' b) = case b of
  Spikes    -> (loc .~ v) . (p_state .~ Falling)
  Web       -> (loc .~ v) . (p_state .~ Stunned)
  PowderKeg -> hp  .~ 0
  Exit      -> loc .~ v  
  _         -> id

moveToEntity v (Enemy' e     ) = case e of
  BigSpider       -> hp -~ 2
  Arrow True      -> hp -~ 2
  Arrow False     -> id 
  Shopkeeper True -> (hp -~ 1) . (p_state .~ Stunned)
  Boulder True    -> hp -~ 5
  Boulder False   -> id
  _               -> hp -~ 1

moveToEntity v (GroundItem' g) = case g of
  Floor c -> pickupConsumable c . (loc .~ v)
  _       -> loc .~ v

moveToEntity v _ = loc .~ v