{-# LANGUAGE TemplateHaskell #-}
module Game(
  stepGame,
  game,
  prompt,
  runCommand,
  runGame,
  Global,
  Textlunky
)

where

import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Types
import Control.Monad(forever)
import TextlunkyParser
import Control.Process
import Control.Lens hiding ((<.>))
import Prelude hiding (round)

makeLenses ''GameState

-- | See below for details
runGame :: GameState -> IO ()
runGame gs = flip evalStateT gs $ (runCommand game)

-- |  What we wnat to do here is:
-- |  0. Print a description of the room the player is in
-- |  1. Get input from the user (using the IO monad)
-- |  2. Build a Free TextlunkyCommand from it (using the Free monad)
-- |  3. Modify the game based on the above Free monad (Using StateT GameState)
-- |  4. Print out user command (using runCommand)
-- |  5. Step the rest of the game (Modify the game state using 
-- |     a function GameState -> GameState)
-- |  6. Repeat.
game :: Textlunky ()
game = forever $ (liftIO $ putStr "> ") >> prompt

-- | Build a textlunky command from the command line
prompt :: Textlunky ()
prompt = liftIO getLine >>= parseInput

-- Process command with access to global state
-- | NB. updateP <.> showP should (will?) update and show each command.
-- | These reside in Control.Process
runCommand :: Textlunky () -> Global GameState ()
runCommand t = do
  cmd <- runFreeT t
  case cmd of
    Pure _   -> return ()
    Free End -> liftIO $ putStrLnP $ "Goodbye!"
    _        -> recursively runCommand (stepGame <.> showP <.> updateP) cmd
