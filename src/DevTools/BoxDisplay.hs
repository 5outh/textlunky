{-# LANGUAGE TemplateHaskell #-}
module DevTools.BoxDisplay(
) where

import Types.Room
import Data.List
import Control.Lens
import Types.Vectors
import Data.Function
import Control.Applicative

makeLenses ''Room

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