{-# LANGUAGE TemplateHaskell #-}
module DevTools.BoxDisplay(
) where

import Types.Room
import Types.Level
import Data.List
import Control.Lens hiding (Level)
import Types.Vectors
import Data.Function
import Control.Applicative
import Data.Maybe(isJust)
import qualified Data.Map as M


makeLenses ''Room
makeLenses ''Level

-- 3d
showBoxed :: (Show a) => [(Vector3 Int, a)] -> String
showBoxed vs = undefined
  where bottoms = filter (\(Vector3 x y z, _) -> z == 0) vs
        tops    = filter (\(Vector3 x y z, _) -> z == 1) vs
        floor   = showInBox bottoms
        ceiling = showInBox tops
-- 2d
-- Note rows will be 0..2
--showInBox :: Show a => [(Vector3 Int, a)] -> String
showInBox vs = undefined
  where longest = maximumBy (compare `on` (length . show . snd)) vs

makeLength :: Int -> String -> String
makeLength n str = let blanks = n - (length str) in str ++ (replicate blanks ' ')

lookupBy :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy _  _ []          = Nothing
lookupBy eq a ((b, x):xs) = if a `eq` b then Just x else lookupBy eq a xs

-- | Show levels with demolished walls, etc.
-- | Normal Room: --
-- |             |  |
-- |              --
-- | Up, Down, East, West
-- | ceilHole, floorHole, wallE, wallW
showLevel :: Level -> String
showLevel l = unlines ( map showRoomAll $ reverse $ threes $ rms )
  where rms = map snd $ M.toList $ l^.rooms -- :: M.Map (Vector2 Int) Room; Note: already sorted!
        threes xs | length xs <= 3 = [xs]
                  | otherwise      = take 3 xs : threes (drop 3 xs)
        showCeil rm = if rm^.ceilHole        then "    " else " -- "
        showFloor rm = if rm^.floorHole      then "    " else " -- "
        showWallE rm = if isJust (rm^.wallE) then " |" else "  "
        showWallW rm = if isJust (rm^.wallW) then "| " else "  "
        showCeilAll  = concatMap showCeil
        showWallsAll = concatMap (\x -> showWallW x ++ showWallE x)
        showFloorAll = concatMap showFloor
        showRoomAll  r = unlines [showCeilAll r, showWallsAll r, showFloorAll r] 