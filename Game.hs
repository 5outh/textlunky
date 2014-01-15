----
-- The main game pipeline, defines flow of control
----

module Game(
  interactGame,
  stepGame,
  game,
  prompt,
  showTIO
)

where

import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Monad.Identity
import Types
import Types.TextlunkyCommand
import Control.Monad(forever)
import Data.Char(toLower)

-- | For some reason this isn't picked up on the import
type Free f = FreeT f Identity 

-- | Modify the game based on user command
-- | TODO: implement
interactGame ::  FreeT TextlunkyCommand IO () 
              -> GameState 
              -> GameState
interactGame _ = id

-- | Step game one round
-- | TODO: implement
stepGame :: GameState -> GameState
stepGame = id

-- | Show current room in GameState
-- | TODO: improve
showRoom :: GameState -> IO ()
showRoom = putStrLn . showGS

-- |  What we wnat to do here is:
-- |  0. Print a description of the room the player is in
-- |  1. Get input from the user (using the IO monad)
-- |  2. Build a Free TextlunkyCommand from it (using the Free monad)
-- |  3. Modify the game based on the above Free monad (Using StateT GameState)
-- |  4. Print out user command (using showCmd)
-- |  5. Step the rest of the game (Modify the game state using 
-- |     a function GameState -> GameState)
-- |  6. Repeat.
-- | (4.) done after unwrapping FreeT in textlunky.hs
game :: StateT GameState (FreeT TextlunkyCommand IO) ()
game = forever $ do
  gs <- get
  lift . lift $ showRoom gs     -- (0.)
  modify (interactGame prompt)  -- (3.) NB. Remember Player in GameState
  modify stepGame               -- (5.)
  lift prompt                   -- (1. & 2.)

-- | Build a command from user prompt
prompt :: FreeT TextlunkyCommand IO ()
prompt = do
  lift $ putStr "~~~~~~~~~~~~~~~\n> "
  cmd <- lift getLine
  case (map toLower cmd) of
    "quit" -> do
      liftF End
      return ()
    "rope" -> do 
      liftF (Rope ())
      return ()
    _      -> lift $ putStrLn "Command not recognized (yet!)."

-- | Print the execution of a user command
showTIO :: FreeT TextlunkyCommand IO r -> IO ()
showTIO t = do
  x <- runFreeT t
  case x of 
     Pure _ -> return ()
     (Free (Move d x)) -> do
      putStrLn $ "You move " ++ show d ++ ".\n"
      showTIO x
     (Free (MoveTo e x)) -> do
      putStrLn $ "You move to the " ++ show e ++ ".\n"
      showTIO x
     (Free (Pickup Nothing x)) -> do
      putStrLn $ "There is nothing to pick up.\n"
      showTIO x
     (Free (Pickup (Just e) x)) -> do
      putStrLn $ "You pick up a " ++ show e ++ ".\n"
      showTIO x
     (Free (DropItem Nothing x)) -> do
      putStrLn $ "You have nothing to drop.\n"
      showTIO x  
     (Free (DropItem (Just e) x)) -> do
      putStrLn $ "You drop your " ++ show e ++ ".\n"
      showTIO x
     (Free (Jump Nothing x)) -> do
      putStrLn $ "You jump in the air.\n"
      showTIO x
     (Free (Jump (Just e) x)) -> do
      putStrLn $ "You jump on a " ++ show e ++ ".\n"
      showTIO x 
     (Free (Attack Nothing x)) -> do
      putStrLn $ "You attack.\n"
      showTIO x
     (Free (Attack (Just e) x)) -> do
      putStrLn $ "You attack a " ++ show e ++ ".\n"
      showTIO x
     (Free (ShootD d x)) -> do
      putStrLn $ "You shoot " ++ show d ++ ".\n"
      showTIO x 
     (Free (ShootE e x)) -> do
      putStrLn $ "You shoot a " ++  show e ++ ".\n"
      showTIO x 
     (Free (ShootSelf x)) -> do
      putStrLn $ "You kill yourself.\n"
      showTIO x
     (Free (Throw d x)) -> do
      putStrLn $ "You throw your item " ++ show d ++ ".\n"
      showTIO x 
     (Free (Rope x)) -> do
      putStrLn $ "You throw a rope up.\n"
      showTIO x 
     (Free (Bomb Nothing x)) -> do
      putStrLn $ "You place a bomb at your feet." ++ ".\n"
      showTIO x 
     (Free (Bomb (Just d) x)) -> do
      putStrLn $ "You place a bomb " ++ 
        (case d of 
          U -> "on the ceiling" -- ??? lol
          D -> "on the floor"
          x -> "near the " ++ show x ++ " wall")
        ++ ".\n"
      showTIO x
     (Free (OpenGoldChest x)) -> do
      putStrLn $ "You open the gold chest.\n" 
      showTIO x
     (Free (OpenChest x)) -> do
      putStrLn $ "You open a chest.\n"
      showTIO x 
     (Free (ExitLevel x)) -> do
      putStrLn $ "You exit the level!\n"
      showTIO x
     (Free (DropDown x)) -> do
      putStrLn $ "You drop down to the next level.\n"
      showTIO x 
     (Free (Look d x)) -> do
      let show' U = "above you"
          show' D = "below you"
          show' x = "to your " ++ show x
      putStrLn $ "You look in the room " ++ show' d ++ ".\n" 
      showTIO x
     (Free End) -> return ()

-- | Sample user command
sample :: Free TextlunkyCommand ()
sample = do
  liftF $ Bomb (Just D) ()
  liftF $ Look U ()
  liftF End