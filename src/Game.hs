{-# LANGUAGE TemplateHaskell #-}
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
import Control.Monad(forever)
import Data.Char(toLower)
import Control.Lens
import TextlunkyParser

-- | An unwrapped `TextlunkyCommand`
type UnwrappedCommand = FreeF TextlunkyCommand () (Textlunky ())

-- | A `Process` used to update the game
type Process = 
     GameState
  -> UnwrappedCommand
  -> Global GameState ()

makeLenses ''GameState
makeLenses ''Player

-- newtype Textlunky r = Textlunky{ runTextlunky :: FreeT TextlunkyCommand (Global GameState) r }

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
  lift . lift $ putStr "> "
  prompt         -- | Get a command
  interactGame   -- | Update game based on commmand
  lift stepGame  -- | Step the game

-- build a textlunky command from the command line
prompt :: Textlunky ()
prompt = do
  cmd <- lift . lift $ getLine
  parseInput cmd :: Textlunky ()

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
  cmd <- runFreeT t
  st  <- get
  case cmd of
    Pure _   -> return ()
    Free End -> lift $ putStrLnP $ "Goodbye!"
    x        -> go showCmd process st cmd

next :: UnwrappedCommand -> Textlunky ()
next (Pure _)                 = return ()
next (Free End)               = return ()
next (Free (Move d x))        = x
next (Free (MoveTo e x))      = x
next (Free (Pickup _ x))      = x
next (Free (DropItem x))      = x
next (Free (Jump _ x))        = x
next (Free (Attack _ x))      = x
next (Free (ShootD _ x))      = x
next (Free (ShootE _ x))      = x
next (Free (ShootSelf x))     = x
next (Free (Throw _ x))       = x
next (Free (Rope x))          = x
next (Free (Bomb _ x))        = x
next (Free (OpenGoldChest x)) = x
next (Free (OpenChest x))     = x
next (Free (ExitLevel x))     = x
next (Free (DropDown x))      = x
next (Free (Look _ x))        = x
next (Free (Walls x))         = x
next (Free (ShowEntities x))  = x
next (Free (ShowFull x))      = x

process :: Process
process st (Pure _)                 = return ()

process st (Free End)               = return ()

process st (Free (Move d x))        = 
  lift $ putStrLnP $ "You move " ++ show d

process st (Free (MoveTo e x))      = 
  lift $ putStrLnP $ "You move to the " ++ show e

process st (Free (Pickup a x))      = case a of
  Just e -> lift $ putStrLnP $ "You pick up a " ++ show e
  _      -> lift $ putStrLnP "There is nothing to pick up."

process st (Free (DropItem x))      = do
  let itm = st^.player^.holding
  lift $ putStrLnP $ case itm of
    Just e ->  "You drop your " ++ show e
    Nothing -> "You have nothing to drop."

process st (Free (Jump a x))        = case a of
  Just e  -> lift $ putStrLnP $ "You jump on a " ++ show e
  Nothing -> lift $ putStrLnP $ "You jump in the air."

process st (Free (Attack a x))      = case a of
  Just e  -> lift $ putStrLnP $ "You jump on a " ++ show e
  Nothing -> lift $ putStrLnP $ "You jump in the air."

process st (Free (ShootD d x))      = 
  lift $ putStrLnP $ "You shoot " ++ show d 

process st (Free (ShootE e x))      = 
  lift $ putStrLnP $ "You shoot a " ++  show e

process st (Free (ShootSelf x))     = 
  lift $ putStrLnP $ "You kill yourself."

process st (Free ( Throw d x))       = 
  lift $ putStrLnP $ "You throw your item " ++ show d

process st (Free (Rope x))          = do
      if st^.player^.ropes > 0 
      then do 
        lift $ putStrLnP "You toss a rope up."
        player.ropes -= 1     -- Weirdly imperative in my Haskell..!
      else lift $ putStrLnP "You don't have any ropes!"

process st (Free (Bomb a x))        = case a of
  Just d  -> 
    lift $ putStrLnP $ "You place a bomb " ++ 
        (case d of 
          U -> "on the ceiling" -- | only with paste...
          D -> "on the floor"
          w -> "near the " ++ show w ++ " wall")
  Nothing -> lift $ putStrLnP $ "You place a bomb at your feet." 

process st (Free (OpenGoldChest x)) = 
  lift $ putStrLnP $ "You open the gold chest." 

process st (Free (OpenChest x))     = 
  lift $ putStrLnP $ "You open a chest."

process st (Free (ExitLevel x))     = 
  lift $ putStrLnP $ "You exit the level!"

process st (Free (DropDown x))      = 
  lift $ putStrLnP $ "You drop down to the next level."

process st (Free (Look d x))        = do
  let show' U = "above you"
      show' D = "below you"
      show' x = "to your " ++ show x
  lift $ putStrLnP $ "You look in the room " ++ show' d

process st (Free (Walls x))         = do
  lift $ putStrLnP $ "You see the following walls:\n"
  lift $ putStrLn  $ showWalls (st^.room._2)

process st (Free (ShowEntities x))  = do
  lift $ putStrLnP $ "You see the following entities:\n"
  lift $ putStrLn  $ showEntities (st^.room._2)

process st (Free (ShowFull x))      = do
  lift $ putStrLnP $ "You see:\n"
  lift $ putStrLn  $ show (st^.room._2)

go :: (Textlunky () -> Global GameState ())
     -> Process
     -> GameState
     -> UnwrappedCommand
     -> Global GameState ()
go f g st cmd = do
  g st cmd
  f (next cmd)
