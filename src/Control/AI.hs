{-# LANGUAGE DeriveFunctor #-}
module Control.AI(
) where

import Data.Vectors
import Data.Direction
import Control.Monad
import Control.Monad.Free

data AIAction a = 
    Move Direction a
  | Attack (Vector3 Int) Int a
  | Explode (Vector3 Int) a
    deriving (Functor, Eq)


type AI = Free AIAction

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