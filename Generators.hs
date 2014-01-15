module Generators(
  randomDir',
  randomDirs',
  randomDir,
  randomDirs
)

where

import qualified System.Random as R
import Types
import Control.Monad.Trans.State
import Control.Monad
import Data.Universe

{- General convenience functions -}

--NB. Get random value from list
randomFromList' :: [a] -> State R.StdGen a
randomFromList' xs = do
  gen <- get
  let (idx, gen') = R.randomR (0, (length xs)) gen
      x = xs !! idx
  put gen'
  return x

--NB. (infinite) list of random values from list
randomsFromList' :: [a] -> State R.StdGen [a]
randomsFromList' = (sequence . repeat) . randomFromList'

--NB. "Just get it for me" generators
randomFromList :: R.StdGen -> [a] -> a
randomFromList gen xs = evalState (randomFromList' xs) gen

randomsFromList :: R.StdGen -> [a] -> [a]
randomsFromList gen xs = evalState (randomsFromList' xs) gen

{-- Direction Generators --}
randomDir' :: State R.StdGen Direction
randomDir' = randomFromList' universe

randomDirs' :: State R.StdGen [Direction]
randomDirs' = randomsFromList' universe

randomDir :: R.StdGen -> Direction
randomDir = evalState randomDir'

randomDirs :: R.StdGen -> [Direction]
randomDirs = evalState randomDirs'

{-- Size --}
randomSize' :: State R.StdGen Size
randomSize' = randomFromList' universe

randomSizes' :: State R.StdGen [Size]
randomSizes' = randomsFromList' universe

randomSize :: R.StdGen -> Size
randomSize = evalState randomSize'

randomSizes :: R.StdGen -> [Size]
randomSizes = evalState randomSizes'

{-- Jewel --}
randomJewel' :: State R.StdGen Jewel
randomJewel' = randomFromList' universe

randomJewels' :: State R.StdGen [Jewel]
randomJewels' = randomsFromList' universe

randomJewel :: R.StdGen -> Jewel
randomJewel = evalState randomJewel'

randomJewels :: R.StdGen -> [Jewel]
randomJewels = evalState randomJewels'

{-- Item --}
randomItem' :: State R.StdGen Item
randomItem' = undefined

randomItems' :: State R.StdGen [Item]
randomItems' = undefined

randomItem :: R.StdGen -> Item
randomItem = undefined

randomItems :: R.StdGen -> [Item]
randomItems = undefined

{-- Consumable --}
randomConsumable' :: State R.StdGen Consumable
randomConsumable' = undefined

randomConsumables' :: State R.StdGen [Consumable]
randomConsumables' = undefined

randomConsumable :: R.StdGen -> Consumable
randomConsumable = undefined

randomConsumables :: R.StdGen -> [Consumable]
randomConsumables = undefined

{-- Block --}
randomBlock' :: State R.StdGen Block
randomBlock' = undefined

randomBlocks' :: State R.StdGen [Block]
randomBlocks' = undefined

randomBlock :: R.StdGen -> Block
randomBlock = undefined

randomBlocks :: R.StdGen -> [Block]
randomBlocks = undefined

{-- RoomType --}
randomRoomType' :: State R.StdGen RoomType
randomRoomType' = undefined

randomRoomTypes' :: State R.StdGen [RoomType]
randomRoomTypes' = undefined

randomRoomType :: R.StdGen -> RoomType
randomRoomType = undefined

randomRoomTypes :: R.StdGen -> [RoomType]
randomRoomTypes = undefined

{-- LevelType --}
randomLevelType' :: State R.StdGen LevelType
randomLevelType' = undefined

randomLevelTypes' :: State R.StdGen [LevelType]
randomLevelTypes' = undefined

randomLevelType :: R.StdGen -> LevelType
randomLevelType = undefined

randomLevelTypes :: R.StdGen -> [LevelType]
randomLevelTypes = undefined

{-- Enemy Generator Tools --}
randomEnemy' :: State R.StdGen Enemy
randomEnemy' = undefined

randomEnemies' :: State R.StdGen [Enemy]
randomEnemies' = undefined

randomEnemy :: R.StdGen -> Enemy
randomEnemy = undefined

randomEnemies :: R.StdGen -> [Enemy]
randomEnemies = undefined

{-- Random GroundItem Tools --}
randomGroundItem' :: State R.StdGen GroundItem
randomGroundItem' = undefined

randomGroundItems' :: State R.StdGen [GroundItem]
randomGroundItems' = undefined

randomGroundItem :: R.StdGen -> GroundItem
randomGroundItem = undefined

randomGroundItems :: R.StdGen -> [GroundItem]
randomGroundItems = undefined

{-- Room Generator Tools --}
randomRoom' :: State R.StdGen Room
randomRoom' = undefined

randomRooms' :: State R.StdGen [Room]
randomRooms' = undefined

randomRoom :: R.StdGen -> Room
randomRoom = undefined

randomRooms :: R.StdGen -> [Room]
randomRooms = undefined

{-- Level Generator Tools --}

randomLevel' :: State R.StdGen Level
randomLevel' = undefined

randomLevel :: R.StdGen -> Level
randomLevel = undefined