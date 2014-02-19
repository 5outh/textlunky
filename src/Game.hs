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

import           Control.Process
import qualified Control.Process as P
import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State
import           Types
import           Control.Monad(forever)
import qualified TextlunkyParser as TP

-- | See below for details
runGame :: GameState -> IO ()
runGame gs = flip evalStateT gs $ (runCommand game)

-- | Get and run each step of the game
game :: Textlunky ()
game = forever $ (liftIO $ putStr "> ") >> prompt

-- | Build a textlunky command from the command line
prompt :: Textlunky ()
prompt = liftIO getLine >>= TP.parseInput

-- | Process command with access to global state
runCommand :: Textlunky () -> Global GameState ()
runCommand t = do
  cmd <- runFreeT t
  case cmd of
    Pure _   -> return ()
    Free End -> liftIO $ putStrLnP $ "Goodbye!"
    _        -> P.recursively runCommand (P.stepGame <.> P.showP <.> P.updateP) cmd
