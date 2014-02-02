module Types.Direction(
  Direction(..),
  Space(..),
  dirs,
  topDirs,
  bottomDirs,
  showRelativeDirection,
  randNSEW,
  randUD,
  randUDM,
  randDir,
  randSpace
) where

import Data.Universe
import Control.Applicative
import Data.List(sort)
import Random.Probability

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
dirs :: [Space]
dirs = triple <$> [D, U] <*> [N, S, M] <*> [E, W, M]
  where triple a b c = (a, b, c)

topDirs, bottomDirs :: [Space]
topDirs    = filter (\(a, _, _) -> a == U) dirs
bottomDirs = filter (\(a, _, _) -> a == D) dirs

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

-- | Random Generation

-- Simple random direction generators (all uniform)
randNSEW, randUD, randUDM, randDir :: (MonadRandom m) => m Direction
randNSEW  = uniform [N, S, E, W]
randUD    = uniform [U, D]
randUDM   = uniform [U, D, M]
randDir   = fromUniverse

-- Random space generator
randSpace :: (MonadRandom m) => m Space
randSpace = uniform dirs