{-# LANGUAGE DeriveFunctor #-}

module Types.TextlunkyCommand(
  TextlunkyCommand(..),
  UserMoves
) where

import Control.Monad.Trans.Free
import Control.Monad.Identity
import Types.Direction
import Types.Enemy
import Types.Entity

data TextlunkyCommand a = 
    Move   Direction a
  | MoveTo Entity a
  | Pickup (Maybe Entity)  a
  | DropItem (Maybe Entity) a
  | Jump   (Maybe Enemy) a
  | Attack (Maybe Enemy) a
  | ShootD Direction a
  | ShootE Enemy a
  | ShootSelf a
  | Throw Direction a
  | Rope a
  | Bomb (Maybe Direction) a
  | OpenGoldChest a
  | OpenChest a
  | ExitLevel a
  | DropDown a
  | Look Direction a
  | End
    deriving (Show, Eq, Functor)

type Free f = FreeT f Identity 
-- | i.e FreeT TextlunkyCommand Identity ()
type UserMoves = Free TextlunkyCommand ()

showCmd :: UserMoves -> String
showCmd x = case runIdentity $ runFreeT x of
 (Free (Move d x)) -> 
  "You move " ++ show d ++ ".\n" ++ showCmd x
 (Free (MoveTo e x)) ->
  "You move to the " ++ show e ++ ".\n" ++ showCmd x
 (Free (Pickup Nothing x)) -> 
  "There is nothing to pick up.\n" ++ showCmd x
 (Free (Pickup (Just e) x)) -> 
  "You pick up a " ++ show e ++ ".\n" ++ showCmd x
 (Free (DropItem Nothing x)) ->
  "You have nothing to drop.\n" ++ showCmd x  
 (Free (DropItem (Just e) x)) ->
  "You drop your " ++ show e ++ ".\n" ++ showCmd x
 (Free (Jump Nothing x)) ->  
  "You jump in the air.\n" ++ showCmd x
 (Free (Jump (Just e) x)) ->  
  "You jump on a " ++ show e ++ ".\n" ++ showCmd x
 (Free (Attack Nothing x)) ->  
  "You attack.\n" ++ showCmd x
 (Free (Attack (Just e) x)) -> 
  "You attack a " ++ show e ++ ".\n" ++ showCmd x
 (Free (ShootD d x)) ->  
  "You shoot " ++ show d ++ ".\n" ++ showCmd x
 (Free (ShootE e x)) ->  
  "You shoot a " ++  show e ++ ".\n" ++ showCmd x
 (Free (ShootSelf x)) -> 
  "You kill yourself.\n" ++ showCmd x
 (Free (Throw d x)) ->  
  "You throw your item " ++ show d ++ ".\n" ++ showCmd x
 (Free (Rope x)) ->  
  "You throw a rope up.\n" ++ showCmd x
 (Free (Bomb Nothing x)) ->  
  "You place a bomb at your feet." ++ ".\n" ++ showCmd x
 (Free (Bomb (Just d) x)) ->  
  "You place a bomb " ++ 
    (case d of 
      U -> "on the ceiling" -- ??? lol
      D -> "on the floor"
      L -> "near the left wall"
      R -> "near the right wall" )
  ++ ".\n" ++ showCmd x
 (Free (OpenGoldChest x)) ->  
  "You open the gold chest.\n" ++ showCmd x
 (Free (OpenChest x)) ->  
  "You open a chest.\n" ++ showCmd x
 (Free (ExitLevel x)) ->  
  "You exit the level!\n" ++ showCmd x
 (Free (DropDown x)) ->  
  "You drop down to the next level.\n" ++ showCmd x
 (Free (Look d x)) ->  
  "You look in the room " ++ show' d ++ ".\n" ++ showCmd x
  where show' U = "above you"
        show' D = "below you"
        show' L = "to your left"
        show' R = "to your right"
 (Free End) -> "~~~~~~~~~~~~~~~~~~~~"

-- PERHAPS THIS REPRESENTS A SINGLE ROUND.
-- YES, I THINK SO.
-- A typical round will parse into:
-- round = do
--   liftF action1
--   liftF action2
--   liftF ...
--   liftF End
sample :: UserMoves
sample = do
  liftF $ Bomb (Just D) ()
  liftF $ Look U ()
  liftF End

