module Types.Synonyms(
	Free(..),
	Global(..),
	Textlunky(..),
	UnwrappedCommand(..),
	Process(..)
) where

import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Monad.Identity
import Types.TextlunkyCommand
import Types.GameState

-- For some reason this isn't picked up on the import
type Free f      = FreeT f Identity 

-- | Some type synonyms
type Global    s = StateT s IO
type Textlunky r = FreeT TextlunkyCommand (Global GameState) r

-- | An unwrapped `TextlunkyCommand`
type UnwrappedCommand = FreeF TextlunkyCommand () (Textlunky ())

-- | A `Process` used to update the game
type Process = 
     GameState
  -> UnwrappedCommand
  -> Global GameState ()
