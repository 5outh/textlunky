----
-- The main game pipeline, defines flow of control
----

module Game(
)

where

import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Monad.Identity
import Types
import Types.TextlunkyCommand
import Control.Lens

-- | Borrowed from the MMorph documentation
generalize :: (Monad m) => Identity a -> m a
generalize = return . runIdentity

-- | What we wnat to do here is:
-- | 0. Print a description of the room the player is in
-- |  1. Get input from the user (using the IO monad)
-- |  2. Build a Free TextlunkyCommand from it (using the Free monad)
-- |  3. Modify the game based on the above Free monad (Using StateT GameState)
-- |  4. Print out user command (using showCmd)
-- |  5. Step the rest of the game (Modify the game state using 
-- |     a function GameState -> GameState)
-- |  6. Repeat.
game :: StateT GameState (FreeT TextlunkyCommand IO) ()
game = do
	gs <- get                                       --get GameState...OK!
	cmd <- lift $ hoistFreeT generalize sample      --lift Free command to FreeT...OK!
	(lift . lift) . putStrLn . showCmd $ return cmd -- show the user command...OK!
	(lift . lift) $ putStrLn "hello world"          -- print stuff...OK!
	modify (id :: GameState -> GameState)           -- modify gamestate...OK!
	return ()                                       -- return ()...OK!