module Types.Direction(
  Direction(..),
  Space(..),
  dirs,
  showRelativeDirection
)

where

import Data.Universe
import Control.Applicative
import Data.List(sort)

data Direction = D | U | N | S | E | W | M  deriving (Bounded, Ord, Eq)

type Space = (Direction, Direction, Direction)

instance Universe Direction where
  universe = [D, U, N, S, E, W, M]

instance Show Direction where
  show N = "north"
  show S = "south"
  show E = "east"
  show W = "west"
  show M = "middle"
  show U = "up"
  show D = "down"

-- | All possible block locations
dirs :: [(Direction, Direction, Direction)]
dirs = triple <$> [D, U] <*> [N, S, M] <*> [E, W, M]
  where triple a b c = (a, b, c)
  
-- | Shows the direction of a space in a room (one of 18 spaces)
showRelativeDirection :: Space -> String
showRelativeDirection (M, M, D) = "dead center"
showRelativeDirection (M, M, U) = "dead center: above"
showRelativeDirection (du, nsm, ewm) = (++du') $  
  case (nsm, ewm) of
    (M, x) -> show x ++ " center"
    (x, M) -> show x ++ " center"
    _      -> show nsm ++ show ewm
    where du' = case du of
                  U -> ": above"
                  _ -> ""