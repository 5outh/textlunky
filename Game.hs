{-# LANGUAGE RankNTypes #-}
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
import Control.Lens

-- i.e. FreeT TextlunkyCommand Identity ()
ftc :: UserMoves
ftc = undefined

generalize :: (Monad m) => Identity a -> m a
generalize = return . runIdentity

runGame :: StateT GameState (FreeT TextlunkyCommand IO) ()
runGame = do
	gs <- get --get GameState...OK!
	(lift . lift) $ putStrLn "hello world" --print stuff...OK!
	return () --return dat ()...OK!