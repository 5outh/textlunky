{-# LANGUAGE TemplateHaskell #-}
module Control.Process.Show(
  showP,
  putStrLnP
) where

import Types
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Lens
import Control.Monad.Trans.Free

makeLenses ''GameState
makeLenses ''Player

-- | Print a string with a prompt token prefix    
putStrLnP = putStrLn . (token++)
  where token = ">> "

-- process handling showing user commands
showP :: Process
showP st (Pure _)  = return ()
showP st (Free End) = return ()

showP st (Free (Move d x)) = 
  liftIO $ putStrLnP $ "You move " ++ show d

showP st (Free (MoveTo e x)) = 
  liftIO $ putStrLnP $ "You move to the " ++ show e

showP st (Free (Pickup a x)) = case a of
  Just e -> liftIO $ putStrLnP $ "You pick up a " ++ show e
  _      -> liftIO $ putStrLnP "There is nothing to pick up."

showP st (Free (DropItem x)) = do
  let itm = st^.player^.holding
  liftIO $ putStrLnP $ case itm of
    Just e ->  "You drop your " ++ show e
    Nothing -> "You have nothing to drop."

showP st (Free (Jump a x)) = case a of
  Just e  -> liftIO $ putStrLnP $ "You jump on a " ++ show e
  Nothing -> liftIO $ putStrLnP $ "You jump in the air."

showP st (Free (Attack a x)) = case a of
  Just e  -> liftIO $ putStrLnP $ "You jump on a " ++ show e
  Nothing -> liftIO $ putStrLnP $ "You jump in the air."

showP st (Free (ShootD d x)) = 
  liftIO $ putStrLnP $ "You shoot " ++ show d 

showP st (Free (ShootE e x)) = 
  liftIO $ putStrLnP $ "You shoot a " ++  show e

showP st (Free (ShootSelf x)) = 
  liftIO $ putStrLnP $ "You kill yourself."

showP st (Free ( Throw d x)) = 
  liftIO $ putStrLnP $ "You throw your item " ++ show d

showP st (Free (Rope x)) = do
      if st^.player^.ropes > 0 
      then do 
        liftIO $ putStrLnP "You toss a rope up."
        player.ropes -= 1     -- Weirdly imperative in my Haskell..!
      else liftIO $ putStrLnP "You don't have any ropes!"

showP st (Free (Bomb a x)) = case a of
  Just d  -> 
    liftIO $ putStrLnP $ "You place a bomb " ++ 
        (case d of 
          U -> "on the ceiling" -- | only with paste...
          D -> "on the floor"
          w -> "near the " ++ show w ++ " wall")
  Nothing -> liftIO $ putStrLnP $ "You place a bomb at your feet."

showP st (Free (OpenGoldChest x)) = 
  liftIO $ putStrLnP $ "You open the gold chest." 

showP st (Free (OpenChest x)) = 
  liftIO $ putStrLnP $ "You open a chest."

showP st (Free (ExitLevel x)) = 
  liftIO $ putStrLnP $ "You exit the level!"

showP st (Free (DropDown x)) = 
  liftIO $ putStrLnP $ "You drop down to the next level."

showP st (Free (Look d x)) = do
  let show' U = "above you"
      show' D = "below you"
      show' x = "to your " ++ show x
  liftIO $ putStrLnP $ "You look in the room " ++ show' d

showP st (Free (Walls x)) = do
  liftIO $ putStrLnP $ "You see the following walls:\n"
  liftIO $ putStrLn  $ showWalls (st^.room._2)

showP st (Free (ShowEntities x)) = do
  liftIO $ putStrLnP $ "You see the following entities:\n"
  liftIO $ putStrLn  $ showEntities (st^.room._2)

showP st (Free (ShowFull x)) = do
  liftIO $ putStrLnP $ "You see:\n"
  liftIO $ putStrLn  $ show (st^.room._2)