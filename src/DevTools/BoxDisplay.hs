{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module DevTools.BoxDisplay(
  showLevelLayout,
  printLevelLayout
) where

import Types.Room
import Types.Level
import Data.List
import Control.Lens hiding (Level)
import Types.Vectors
import Data.Function
import Control.Applicative
import Data.Maybe(isJust)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M

makeLenses ''Room
makeLenses ''Level

-- | Show levels with demolished walls, etc.
-- | Normal Room: --
-- |             |  |
-- |              --
showLevelLayout :: Level -> String
showLevelLayout lvl = 
  unlines ( map showRoomAll $ reverse $ transpose $ threes $ rms )
  where rms = M.toList $ lvl^.rooms
        threes xs | length xs <= 3 = [xs]
                  | otherwise      = take 3 xs : threes (drop 3 xs)
        showCeil  (Vector2 x y, rm) = 
          if rm^.ceilHole       then "    " else " -- "
        showFloor (Vector2 x y, rm) = 
          if rm^.floorHole      then "    " else " -- "
        showWallE (Vector2 x y, rm) = 
          if isJust (rm^.wallE) then [start, '|'] else [start, ' ']
          where start = if rm^.rType == StartRoom then 'S' else ' '
        showWallW (Vector2 x y, rm) = 
          if isJust (rm^.wallW) then [ '|', end] else [' ', end]
          where end = if rm^.rType == LevelExit then 'E' else ' '
        showCeilAll  = concatMap showCeil
        showWallsAll = concatMap (\x -> showWallW x ++ showWallE x)
        showFloorAll = concatMap showFloor
        showRoomAll  r = unlines [showCeilAll r, showWallsAll r, showFloorAll r]

printLevelLayout :: Level -> IO ()
printLevelLayout = putStrLn . showLevelLayout