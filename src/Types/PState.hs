module Types.PState(
  PState(..)
) where

import Types.Direction

data PState = Standing
            | Falling
            | Stunned
            | Whipping
            | Shooting Direction
             deriving (Show, Eq)