module Game(
  interactGame,
  stepGame,
  game,
  prompt,
  showCmd,
  runGame,
  Global,
  Textlunky
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
type Free f      = FreeT f Identity 

-- | Some type synonyms
type Global    s = StateT s IO
type Textlunky r = FreeT TextlunkyCommand (Global GameState) r

-- | Print a string with a prompt token prefix    
putStrLnP = putStrLn . (token++)
  where token = ">> "

-- | See below for details
runGame :: GameState -> IO ()
runGame gs = flip evalStateT gs $ showCmd game

-- |  What we wnat to do here is:
-- |  0. Print a description of the room the player is in
-- |  1. Get input from the user (using the IO monad)
-- |  2. Build a Free TextlunkyCommand from it (using the Free monad)
-- |  3. Modify the game based on the above Free monad (Using StateT GameState)
-- |  4. Print out user command (using showCmd)
-- |  5. Step the rest of the game (Modify the game state using 
-- |     a function GameState -> GameState)
-- |  6. Repeat.
-- | Build a command from user prompt
game :: Textlunky ()
game = forever $ do
  lift printRoom -- | Print current room
  prompt         -- | Get a command
  interactGame   -- | Update game based on commmand
  lift stepGame  -- | Step the game

-- build a textlunky command from the command line
prompt :: Textlunky ()
prompt = do
  lift . lift $ putStr "> "
  cmd <- lift . lift $ getLine
  case parseCmd (map toLower cmd) of
    Just x -> x
    _      -> prompt

-- | NOTE: This will go in the parser module and will use
-- |       Parsec for parsing, but kept simple for now
parseCmd :: String -> Maybe (Textlunky ())
parseCmd "quit" = Just $ liftF End
parseCmd "rope" = Just $ liftF (Rope ())
parseCmd "test" = Just $ 
  do liftF (Rope ())
     liftF (Move E ())
parseCmd _      = Nothing

-- | Show current room in GameState
-- | TODO: improve
printRoom :: Global GameState ()
printRoom = do
  st <- get
  lift . putStrLn $ showGS st

-- | Modify the game based on user command
-- | TODO: implement
interactGame :: Textlunky ()
interactGame = return ()

-- | Step game one round
-- | TODO: implement
stepGame :: Global GameState ()
stepGame = return ()

-- Show a command with access to global state
showCmd :: Textlunky () -> Global GameState ()
showCmd t = do
  x  <- runFreeT t
  st <- get
  case x of
    Pure _ -> return ()
    (Free (Move d x)) -> do
     lift $ putStrLnP $ "You move " ++ show d 
     showCmd x
    (Free (MoveTo e x)) -> do
     lift $ putStrLnP $ "You move to the " ++ show e
     showCmd x
    (Free (Pickup Nothing x)) -> do
     lift $ putStrLnP "There is nothing to pick up."
     showCmd x
    (Free (Pickup (Just e) x)) -> do
     lift $ putStrLnP $ "You pick up a " ++ show e
     showCmd x
    (Free (DropItem Nothing x)) -> do
     lift $ putStrLnP $ "You have nothing to drop."
     showCmd x  
    (Free (DropItem (Just e) x)) -> do
     lift $ putStrLnP $ "You drop your " ++ show e
     showCmd x
    (Free (Jump Nothing x)) -> do
     lift $ putStrLnP $ "You jump in the air."
     showCmd x
    (Free (Jump (Just e) x)) -> do
     lift $ putStrLnP $ "You jump on a " ++ show e
     showCmd x 
    (Free (Attack Nothing x)) -> do
     lift $ putStrLnP $ "You attack."
     showCmd x
    (Free (Attack (Just e) x)) -> do
     lift $ putStrLnP $ "You attack a " ++ show e
     showCmd x
    (Free (ShootD d x)) -> do
     lift $ putStrLnP $ "You shoot " ++ show d 
     showCmd x 
    (Free (ShootE e x)) -> do
     lift $ putStrLnP $ "You shoot a " ++  show e
     showCmd x 
    (Free (ShootSelf x)) -> do
     lift $ putStrLnP $ "You kill yourself."
     showCmd x
    (Free (Throw d x)) -> do
     lift $ putStrLnP $ "You throw your item " ++ show d
     showCmd x 
    (Free (Rope x)) -> do
     lift $ putStrLnP $ "You toss a rope up."
     showCmd x 
    (Free (Bomb Nothing x)) -> do
     lift $ putStrLnP $ "You place a bomb at your feet." 
     showCmd x 
    (Free (Bomb (Just d) x)) -> do
     lift $ putStrLnP $ "You place a bomb " ++ 
       (case d of 
         U -> "on the ceiling" -- | only with paste...
         D -> "on the floor"
         x -> "near the " ++ show x ++ " wall")
     showCmd x
    (Free (OpenGoldChest x)) -> do
     lift $ putStrLnP $ "You open the gold chest." 
     showCmd x
    (Free (OpenChest x)) -> do
     lift $ putStrLnP $ "You open a chest."
     showCmd x 
    (Free (ExitLevel x)) -> do
     lift $ putStrLnP $ "You exit the level!"
     showCmd x
    (Free (DropDown x)) -> do
     lift $ putStrLnP $ "You drop down to the next level."
     showCmd x 
    (Free (Look d x)) -> do
     let show' U = "above you"
         show' D = "below you"
         show' x = "to your " ++ show x
     lift $ putStrLnP $ "You look in the room " ++ show' d
     showCmd x
    (Free End) -> lift $ putStrLnP "Goodbye!" >> return ()
