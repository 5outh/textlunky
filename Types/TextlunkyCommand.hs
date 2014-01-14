{-# LANGUAGE DeriveFunctor #-}

module Types.TextlunkyCommand(
  TextlunkyCommand(..),
  UserMoves
) where

import Control.Monad.Free
import Types.Direction
import Types.Enemy
import Types.Entity

-- NB. Nothing => default behavior defined in notes.md
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
  | Concurrent (TextlunkyCommand a) a -- | &
  | ExitLevel a
  | DropDown a
  | Look Direction a
  | End
    deriving (Show, Eq, Functor)

type UserMoves = Free TextlunkyCommand ()

showCmd :: UserMoves -> String
showCmd (Free (Move d x)) = 
  "You move " ++ show d ++ ".\n" ++ showCmd x
showCmd (Free (MoveTo e x)) =
  "You move to the " ++ show e ++ ".\n" ++ showCmd x
showCmd (Free (Pickup Nothing x)) = 
  "There is nothing to pick up.\n" ++ showCmd x
showCmd (Free (Pickup (Just e) x)) = 
  "You pick up a " ++ show e ++ ".\n" ++ showCmd x
showCmd (Free (DropItem Nothing x)) =
  "You have nothing to drop.\n" ++ showCmd x  
showCmd (Free (DropItem (Just e) x)) =
  "You drop your " ++ show e ++ ".\n" ++ showCmd x
showCmd (Free (Jump Nothing x)) =  
  "You jump in the air.\n" ++ showCmd x
showCmd (Free (Jump (Just e) x)) =  
  "You jump on a " ++ show e ++ ".\n" ++ showCmd x
showCmd (Free (Attack Nothing x)) =  
  "You attack.\n" ++ showCmd x
showCmd (Free (Attack (Just e) x)) = 
  "You attack a " ++ show e ++ ".\n" ++ showCmd x
showCmd (Free (ShootD d x)) =  
  "You shoot " ++ show d ++ ".\n" ++ showCmd x
showCmd (Free (ShootE e x)) =  
  "You shoot a " ++  show e ++ ".\n" ++ showCmd x
showCmd (Free (ShootSelf x)) = 
  "You kill yourself.\n" ++ showCmd x
showCmd (Free (Throw d x)) =  
  "You throw your item " ++ show d ++ ".\n" ++ showCmd x
showCmd (Free (Rope x)) =  
  "You throw a rope up.\n" ++ showCmd x
showCmd (Free (Bomb Nothing x)) =  
  "You place a bomb at your feet." ++ ".\n" ++ showCmd x
showCmd (Free (Bomb (Just d) x)) =  
  "You bomb the " ++ 
    (case d of 
      U -> "ceiling"
      D -> "floor"
      L -> "left wall"
      R -> "right wall" )
  ++ ".\n" ++ showCmd x
showCmd (Free (OpenGoldChest x)) =  
  "You open the gold chest.\n" ++ showCmd x
showCmd (Free (OpenChest x)) =  
  "You open a chest.\n" ++ showCmd x
-- we'll see!
showCmd (Free (Concurrent t x)) =
  showCmd (Free t) ++ showCmd x
showCmd (Free (ExitLevel x)) =  
  "You exit the level!\n" ++ showCmd x
showCmd (Free (DropDown x)) =  
  "You drop down to the next level.\n" ++ showCmd x
showCmd (Free (Look d x)) =  
  "You look in the room " ++ show' d ++ ".\n" ++ showCmd x
  where show' U = "above you"
        show' D = "below you"
        show' L = "to your left"
        show' R = "to your right"
showCmd (Free End) = ""

sample :: UserMoves
sample = do
  liftF $ Concurrent End ()
  liftF $ Look U ()
  liftF End

