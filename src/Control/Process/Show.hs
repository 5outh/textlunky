{-# LANGUAGE TemplateHaskell #-}
module Control.Process.Show(
  showP,
  putStrLnP
) where

import Types
import Control.Process.Class

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Lens
import Control.Monad.Trans.Free
import qualified Data.Map as M

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Room

-- | Print a string with a prompt token prefix    
putStrLnP = putStrLn . (token++)
  where token = ">> "

-- process handling showing user commands
showP :: Process
showP (Pure _)  = return ()
showP (Free End) = return ()

showP (Free (Move d x)) = 
  liftIO $ putStrLnP $ "You move " ++ show d

showP (Free (MoveTo e x)) = 
  liftIO $ putStrLnP $ "You move to the " ++ show e

-- TODO: Make sure we can actually pick up the things we're picking up...
showP (Free (Pickup a x)) = case a of
  Just e -> liftIO $ putStrLnP $ "You pick up a " ++ show e
  _      -> do 
    loc  <- use $ player.loc
    hdg  <- use $ player.holding
    ents <- use $ room._2.entities -- stuff in the current room
    case M.lookup loc ents of
      Just x  -> liftIO $ putStrLnP $ 
        case hdg of
          Just h -> "You drop your " ++ show h ++ " and pick up " ++ show x ++ "."
          _      -> "You pick up "   ++ show x ++ "."
      Nothing -> liftIO $ putStrLnP "There is nothing to pick up."

showP (Free (DropItem x)) = do
  itm <- use $ player.holding
  liftIO $ putStrLnP $ case itm of
    Just e ->  "You drop your " ++ show e
    Nothing -> "You have nothing to drop."

showP (Free (Jump a x)) = case a of
  Just e  -> liftIO $ putStrLnP $ "You jump on a " ++ show e
  Nothing -> liftIO $ putStrLnP $ "You jump in the air."

showP (Free (Attack a x)) = case a of
  Just e  -> liftIO $ putStrLnP $ "You jump on a " ++ show e
  Nothing -> liftIO $ putStrLnP $ "You jump in the air."

showP (Free (ShootD d x)) = 
  liftIO $ putStrLnP $ "You shoot " ++ show d 

showP (Free (ShootE e x)) = 
  liftIO $ putStrLnP $ "You shoot a " ++  show e

showP (Free (ShootSelf x)) = 
  liftIO $ putStrLnP $ "You kill yourself."

showP (Free (Throw d x)) = 
  liftIO $ putStrLnP $ "You throw your item " ++ show d

showP (Free (Rope x)) = do
    rps <- use $ player.ropes
    if rps > 0 
    then liftIO $ putStrLnP "You toss a rope up."
    else liftIO $ putStrLnP "You don't have any ropes!"

-- | TODO: Can place bomb "near the middle wall" right now...
showP (Free (Bomb a x)) = do
  plr <- use player
  let str = case a of
              Just d  -> case d of
                U -> if Paste `elem` (plr^.items) 
                     then "You place a bomb on the ceiling."
                     else "You find yourself unable to get the bomb to stay on the ceiling..."
                D -> "You place a bomb on the floor."
                w -> "You place a bomb near the " ++ show w ++ " wall."
              Nothing -> "You place a bomb at your feet."
  liftIO $ putStrLn $ if plr^.bombs > 0 then str else "You don't have any bombs!"

-- still need to validate if Gold Chest is even in the room.
showP (Free (OpenGoldChest x)) = do
  hdg <- use $ player.holding
  liftIO $ putStrLnP $
    case hdg of
      Just (GroundItem' Key) -> 
        "You open the gold chest! Something shiny pops out." 
      _                      -> 
        "It's locked! There may be a key laying around here somewhere..."

showP (Free (OpenChest x)) = 
  liftIO $ putStrLnP $ "You open a chest."

showP (Free (ExitLevel x)) = 
  liftIO $ putStrLnP $ "You exit the level!"

showP (Free (DropDown x)) = 
  liftIO $ putStrLnP $ "You drop down to the next level."

showP (Free (Look d x)) = do
  let show' U = "above you"
      show' D = "below you"
      show' x = "to your " ++ show x
  liftIO $ putStrLnP $ "You look in the room " ++ show' d

showP (Free (Walls x)) = do
  rm <- use $ room._2
  liftIO $ putStrLnP $ "You see the following walls:\n"
  liftIO $ putStrLn  $ showWalls rm

showP (Free (ShowEntities x)) = do
  rm <- use $ room._2
  liftIO $ putStrLnP $ "You see the following things:\n"
  liftIO $ putStrLn  $ showEntitiesWithIds rm

showP (Free (ShowFull x)) = do
  rm <- use $ room._2
  liftIO $ putStrLnP $ "You see:\n"
  liftIO $ putStrLn  $ show rm

showP (Free (ShowMe x)) = do
  plr <- use player
  liftIO $ putStrLnP $ show plr

showP (Free (YOLO x))   = do
  liftIO $ putStrLnP $ concat $
    [ "You start flailing your arms in the air,",
      " wailing \"YOLO\" at the top of your lungs.",
      " You throw your remaining bombs and ropes around the room ",
      " in a fit of joy. YOLO, right?"
    ]