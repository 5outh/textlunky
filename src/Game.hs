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
-- | NB. Surprisingly, this does not process `prompt` twice 
--       even though it shows up in the declaration twice.
game :: StateT GameState (FreeT TextlunkyCommand IO) ()
game = forever $ do
  gs <- get
  lift . lift $ showRoom gs   
  modify (interactGame prompt)
  modify stepGame             
  lift prompt                 

-- | Build a command from user prompt
prompt :: FreeT TextlunkyCommand IO ()
prompt = do
  lift $ putStr "\n> "
  cmd <- lift getLine
  case (map toLower cmd) of
    "quit" -> do
      liftF End
      return ()
    "rope" -> do 
      liftF (Rope ())
      return ()
    _      -> lift $ putStrLnP "Command not recognized (yet!)."

{---- @FIX -------}

game' :: FreeT TextlunkyCommand (StateT GameState IO) ()
game' = do
  prompt'        -- | Get a command
  lift printRoom -- | Print current room
  showCmd        -- | Show the current command based on state
  interactGame'  -- | Update game based on commmand
  lift stepGame' -- | Step the game

prompt' :: FreeT TextlunkyCommand (StateT GameState IO) ()
prompt' = do
  lift . lift $ putStr "\n>"
  cmd <- lift . lift $ getLine
  case (map toLower cmd) of
    "quit" -> do
      liftF End
    "rope" -> do
      liftF (Rope ())
    _ -> lift . lift $ putStrLnP "Command not recognized."

printRoom :: StateT GameState IO ()
printRoom = do
  st <- get
  lift . putStrLn $ showGS st

-- get a command, modify the inner state based on it
interactGame' :: FreeT TextlunkyCommand (StateT GameState IO) ()
interactGame' = undefined

-- step the game (irrelevant of command)
stepGame' :: StateT GameState IO ()
stepGame' = undefined

-- show the command, in the context of current game state
showCmd :: FreeT TextlunkyCommand (StateT GameState IO) ()
showCmd  = undefined

main' = do
  let gs = undefined :: GameState
  flip runStateT gs $ runFreeT game'
  return ()

{---- @ENDFIX ----}
-- | Print a string with a prompt token prefix    
putStrLnP = putStrLn . (token++)
  where token = ">> "
  
-- | Print the execution of a user command
showTIO :: FreeT TextlunkyCommand IO r -> IO ()
showTIO t = do
  x <- runFreeT t
  case x of
     Pure _ -> return ()
     (Free (Move d x)) -> do
      putStrLnP $ "You move " ++ show d ++ ".\n"
      showTIO x
     (Free (MoveTo e x)) -> do
      putStrLnP $ "You move to the " ++ show e ++ ".\n"
      showTIO x
     (Free (Pickup Nothing x)) -> do
      putStrLnP $ "There is nothing to pick up.\n"
      showTIO x
     (Free (Pickup (Just e) x)) -> do
      putStrLnP $ "You pick up a " ++ show e ++ ".\n"
      showTIO x
     (Free (DropItem Nothing x)) -> do
      putStrLnP $ "You have nothing to drop.\n"
      showTIO x  
     (Free (DropItem (Just e) x)) -> do
      putStrLnP $ "You drop your " ++ show e ++ ".\n"
      showTIO x
     (Free (Jump Nothing x)) -> do
      putStrLnP $ "You jump in the air.\n"
      showTIO x
     (Free (Jump (Just e) x)) -> do
      putStrLnP $ "You jump on a " ++ show e ++ ".\n"
      showTIO x 
     (Free (Attack Nothing x)) -> do
      putStrLnP $ "You attack.\n"
      showTIO x
     (Free (Attack (Just e) x)) -> do
      putStrLnP $ "You attack a " ++ show e ++ ".\n"
      showTIO x
     (Free (ShootD d x)) -> do
      putStrLnP $ "You shoot " ++ show d ++ ".\n"
      showTIO x 
     (Free (ShootE e x)) -> do
      putStrLnP $ "You shoot a " ++  show e ++ ".\n"
      showTIO x 
     (Free (ShootSelf x)) -> do
      putStrLnP $ "You kill yourself.\n"
      showTIO x
     (Free (Throw d x)) -> do
      putStrLnP $ "You throw your item " ++ show d ++ ".\n"
      showTIO x 
     (Free (Rope x)) -> do
      putStrLnP $ "You toss a rope up.\n"
      showTIO x 
     (Free (Bomb Nothing x)) -> do
      putStrLnP $ "You place a bomb at your feet." ++ ".\n"
      showTIO x 
     (Free (Bomb (Just d) x)) -> do
      putStrLnP $ "You place a bomb " ++ 
        (case d of 
          U -> "on the ceiling" -- | only with paste...
          D -> "on the floor"
          x -> "near the " ++ show x ++ " wall")
        ++ ".\n"
      showTIO x
     (Free (OpenGoldChest x)) -> do
      putStrLnP $ "You open the gold chest.\n" 
      showTIO x
     (Free (OpenChest x)) -> do
      putStrLnP $ "You open a chest.\n"
      showTIO x 
     (Free (ExitLevel x)) -> do
      putStrLnP $ "You exit the level!\n"
      showTIO x
     (Free (DropDown x)) -> do
      putStrLnP $ "You drop down to the next level.\n"
      showTIO x 
     (Free (Look d x)) -> do
      let show' U = "above you"
          show' D = "below you"
          show' x = "to your " ++ show x
      putStrLnP $ "You look in the room " ++ show' d ++ ".\n" 
      showTIO x
     (Free End) -> putStrLnP "Goodbye!" >> return ()

-- | Sample user command
sample :: Free TextlunkyCommand ()
sample = do
  liftF $ Bomb (Just D) ()
  liftF $ Look U ()
  liftF End