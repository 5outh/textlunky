module Control.Process(
  idP,
  (<.>),
  next,
  recursively,

  module Control.Process.Show,
  module Control.Process.Update,
  module Control.Process.Class
) where

import Control.Process.Show
import Control.Process.Update
import Control.Process.Class

import Types.Synonyms
import Types.TextlunkyCommand
import Types.GameState
import Control.Monad.Trans.Free

-- NB. if we change the type to:
-- type Process state cmd r = 
--   FreeF cmd r () (FreeT cmd (state) r) -> StateT state IO r
-- then the current Process is:
-- Process GameState TextlunkyCommand ()
-- which is interesting because we might be able to form something new from it,
-- but this is on the backburner because I don't really care that much. 
-- Just think it's interesting for discussion later.

-- process Id
idP :: Process
idP = const $ return ()

infixr 9 <.>

(<.>) :: Process -> Process -> Process
pA <.> pB = \cmd -> pA cmd >> pB cmd

recursively :: (Textlunky () -> Global GameState ())
            -> Process
            -> UnwrappedCommand
            -> Global GameState ()
recursively f g cmd = do
  g cmd
  f (next cmd)

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
next (Free (ShowMe x))        = x
next (Free (YOLO x))          = x