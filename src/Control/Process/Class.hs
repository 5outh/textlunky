module Control.Process.Class(
	UnwrappedCommand(..),
	Process(..)
) where

import Types
import Control.Monad.Trans.Free
import Control.Monad.State.Class

-- | An unwrapped `TextlunkyCommand`
type UnwrappedCommand = FreeF TextlunkyCommand () (Textlunky ())

-- | A `Process` used to update the game
type Process = UnwrappedCommand -> Global GameState ()
