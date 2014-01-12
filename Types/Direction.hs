module Types.Direction(
  Direction(..),
  Space(..),
  dirs,
  srt,
  showRelativeDirection
)

where

import Data.Universe
import Control.Applicative
import Data.List(sort)

data Direction = U  | D | M | L | R deriving (Bounded, Ord, Eq)

type Space = (Direction, Direction)

instance Universe Direction where
  universe = [U, D, L, R, M]
  
instance Show Direction where 
  show U = "up"
  show D = "down"
  show L = "left"
  show R = "right"
  show M = "middle"

--NB. All block locations
dirs :: [(Direction, Direction)]
dirs = (,) <$> [L, R, M] <*> [M, U, D]  

-- NB. This is used in `showRelativeDirection`
srt :: (Ord a) => (a, a) -> (a, a)
srt (a, b) = let [a', b'] = sort [a, b] in (a', b')
  
-- NB. We assume U, D will never be a thing, L, R will never be a thing. So x in {U, D, M}, y in {M, L, R} (ordered sets)
-- | Shows the direction of a space in a room (one of nine spaces)
showRelativeDirection :: Space -> String
showRelative (M, M) = "dead center"
showRelativeDirection a = h ++ " " ++ j
  where (x, y) = srt a
        h = case x of 
            U -> "upper"
            D -> "lower"
            M -> "middle"
        j = case y of 
            L -> "left"
            R -> "right"
            M -> "middle"