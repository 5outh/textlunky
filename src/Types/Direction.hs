module Types.Direction(
  Direction(..),
  Space(..), 
  moveVertical,
  dirs,
  topDirs,
  bottomDirs,
  showRelativeDirection,
  randNSEW,
  randUD,
  randUDM,
  randDir,
  randSpace,
  fromVector3,
  toVector3
) where

import Data.Universe
import Control.Applicative
import Data.List(sort)
import Random.Probability
import Types.Vectors

data Direction = D | U | N | S | E | W | M  deriving (Bounded, Ord, Eq)

type Space = (Direction, Direction, Direction)

-- d in {EWUD}
moveVertical :: Direction -> (Direction, Direction) -> (Direction, Direction)
moveVertical U (x, y) = (x, y')
  where y' = case y of 
               D -> M
               M -> U
               U -> U
               _ -> y
moveVertical D (x, y) = (x, y')
  where y' = case y of 
               D -> D
               M -> D
               U -> M
               _ -> y
moveVertical E (x, y) = (x', y)
  where x' = case x of 
               E -> E
               W -> M
               M -> E
               _ -> y
moveVertical W (x, y) = (x', y)
  where x' = case x of 
               E -> M
               W -> W
               M -> W
               _ -> y

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

fromVector3 :: (Integral a, Show a) => Vector3 a -> Space
fromVector3 (Vector3 x y z) = (z', y', x')
  where x' = case x of
              0 -> E 
              1 -> M
              2 -> W
              a -> error $ "Invalid X value" ++ show a ++ "in Vector3"
        y' = case y of
              0 -> S
              1 -> M
              2 -> N
              a -> error $ "Invalid Y value" ++ show a ++ "in Vector3"
        z' = case z of
              0 -> D
              1 -> U
              a -> error $ "Invalid Z value" ++ show a ++ "in Vector3"

toVector3 :: Space -> Vector3 Int
toVector3 (du, nsm, ewm) = Vector3 x y z
  where x = case ewm of 
              E -> 0
              M -> 1
              W -> 2
              a -> error $ "Invalid x value" ++ show a ++ "in Vector3"
        y = case nsm of 
              S -> 0
              M -> 1
              N -> 2
              a -> error $ "Invalid y value" ++ show a ++ "in Vector3"
        z = case du of 
              D -> 0
              U -> 1
              a -> error $ "Invalid z value" ++ show a ++ "in Vector3"

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