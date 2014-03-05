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
import           Control.Monad(unless)
import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State
import           Types
import           Control.Monad(forever)
import qualified TextlunkyParser as TP
import qualified Data.Map as M
import           Control.Lens hiding (Level, (<.>))

makeLenses ''GameState
makeLenses ''Level
makeLenses ''Room
makeLenses ''Player

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
  health <- use $ player.hp
  unless (health <= 0) $ do
    cmd <- runFreeT t
    case cmd of
      Pure _   -> return ()
      Free End -> liftIO $ putStrLnP $ "Goodbye!"
      _        -> do
          P.recursively runCommand (P.showP <.> P.updateP <.> P.stepGame) cmd
