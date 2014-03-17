module Data.Direction(
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
import Data.Vectors
import Data.Maybe(fromJust, isJust)

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
dirs = triple <$> [E, W, M] <*> [N, S, M] <*> [D, U]
  where triple a b c = (a, b, c)

topDirs, bottomDirs :: [Space]
topDirs    = filter (\(_, _, a) -> a == U) dirs
bottomDirs = filter (\(_, _, a) -> a == D) dirs

fromVector3 :: (Integral a, Show a) => Vector3 a -> Space
fromVector3 (Vector3 x y z) = (x', y', z')
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

toVector3 :: Space -> Maybe (Vector3 Int)
toVector3 (ewm, nsm, du) = v
  where x = case ewm of 
              E -> Just 0
              M -> Just 1
              W -> Just 2
              a -> Nothing
        y = case nsm of 
              S -> Just 0
              M -> Just 1
              N -> Just 2
              a -> Nothing
        z = case du of 
              D -> Just 0
              U -> Just 1
              a -> Nothing
        v = if all isJust [x, y, z] 
            then Just $ Vector3 (fromJust x) (fromJust y) (fromJust z)
            else Nothing 

-- | Shows the direction of a space in a room (one of 18 spaces)
showRelativeDirection :: Space -> String
showRelativeDirection (M, M, D) = "dead center"
showRelativeDirection (M, M, U) = "dead center: above"
showRelativeDirection (ewm, nsm, du) = (++du') $  
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