{-# LANGUAGE DeriveFunctor #-}
module EventProcessing(
) where

import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.Identity
import Control.Monad.Trans.State

-- super generic for right now. Only one type of event, for testing.
data Event a next = Event a next deriving Functor

-- also super boring. State is just an Int.
type GState = Int

-- Free (Event Char) ()
eventGen :: (Monad m) => FreeT (Event Char) m ()
eventGen = undefined

type Game = FreeT (Event Char) (StateT GState IO) ()

runGame :: Game -> IO ()
runGame = undefined

--game :: Game
game t = undefined