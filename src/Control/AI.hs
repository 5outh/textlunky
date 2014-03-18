{-# LANGUAGE DeriveFunctor #-}
module Control.AI(
) where

import Data.Vectors
import Data.Direction
import Types.GameState
import Types.Entity
import Types.Synonyms hiding (Free)
import Control.Monad
import Control.Monad.Free

data AIAction a = 
    Move Direction a
  | Attack (Vector3 Int) Int a
  | Explode (Vector3 Int) a
  | End
    deriving (Functor, Eq)


type AI = Free AIAction
type AIUpdater a = AI () -> a -> a
-- Update process for the entire game state
type AIProcess  = AIUpdater GameState
-- Update process for the entity in question
type AIModifier = AIUpdater Entity

moveAI :: Direction -> AI ()
moveAI d = liftF ( Move d () )

attackAI :: Vector3 Int -> Int -> AI ()
attackAI v dmg = liftF ( Attack v dmg () )

explodeAI :: Vector3 Int -> AI ()
explodeAI v = liftF ( Explode v () )

moveDirs :: [Direction] -> AI ()
moveDirs = forever . mapM_ (replicateM 3 . moveAI)

moveEW :: AI ()
moveEW = moveDirs [E, W]

moveNS :: AI ()
moveNS = moveDirs [N, S]

moveUD :: AI ()
moveUD = moveDirs [U, D]

processAction :: AIProcess
processAction (Free (Move _ a))       gs = processAction a gs
processAction (Free (Attack v dmg a)) gs = processAction a gs
processAction (Free (Explode v a))    gs = processAction a gs
processAction (Free End)              gs = gs