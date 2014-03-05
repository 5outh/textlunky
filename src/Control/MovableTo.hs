{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
module Control.MovableTo(
  MovableTo(..)
) where

import Types.Vectors
import Types.Jewel
import Types.Player
import Control.Lens

makeLenses ''Player

-- NB. Vector type is previously inhabited space 
-- Takes an vector (`v x`, prev. location), 
-- something that can be moved onto `a`
-- and produces a function that modifies a movable type `b`
class (Vector v) => MovableTo v a b where
  onMoveTo :: v x -> a -> (b -> b)

-- since can move into a Jewel space, don't really care what the old vector is
instance MovableTo Vector2 Jewel Player where
  onMoveTo _ j = gold +~ (value j)