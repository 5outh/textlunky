module Types.PState(
  PState(..)
) where

import Data.Direction

data PState = Standing
            | Falling
            | Stunned
            | Whipping
            | Shooting Direction
             deriving (Show, Eq)