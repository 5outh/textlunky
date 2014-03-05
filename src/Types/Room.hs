{-# LANGUAGE TemplateHaskell #-}
module Types.Room(
  Room(..),
  RoomType(..),
  randMinesRoom,
  demolish,
  addLadderU,
  addLadderD,
  showWalls,
  showEntities,
  showEntitiesWithIds,
  addEntityAt,
  getEntityAt
) where

import Data.Default
import Data.Universe
import Data.List(intercalate)
import Data.Maybe(fromJust)
import Types.Direction
import Types.Entity
import Types.Item
import Types.Wall
import Types.Vectors
import Control.Lens
import Control.Monad(replicateM, liftM)
import Random.Probability
import qualified Data.Map as M

data RoomType = NormalRoom
              | KaliAltar
              | Shop
              | LevelExit
              | StartRoom
                deriving (Enum, Eq)


data Room = Room{
      _entities  :: M.Map (Vector3 Int) Entity
    , _rType     :: RoomType   
    , _ladderU   :: Bool      
    , _ladderD   :: Bool      
    , _wallN     :: Maybe Wall
    , _wallS     :: Maybe Wall
    , _wallE     :: Maybe Wall
    , _wallW     :: Maybe Wall
    , _ceilHole  :: Bool      
    , _floorHole :: Bool      
} deriving (Eq)

makeLenses ''Room

-- full room show
instance Show Room where
    show r = concat $ 
      (walls : ladders : holes : t : map show' (M.toList (r^.entities)))
        where show' (spc, entity) = 
                   "There is "  
                ++ show entity 
                ++ " in the " 
                ++ showRelativeDirection (fromVector3 spc) 
                ++ ".\n"
              t = case (r^.rType) of
                KaliAltar      -> "You see an altar to Kali.\n"
                Shop           -> "You have found a shop!\n"
                LevelExit      -> "You have found the exit!\n"
                _         -> []
              ladders = case (r^.ladderU, r^.ladderD) of
                (True, True) -> "There are ladders both up and down.\n"
                (True, _   ) -> "There is a ladder up.\n"
                (_   , True) -> "There is a ladder down.\n"
                _            -> []
              holes   = case (r^.ceilHole, r^.floorHole) of
                (True, True) -> "There are holes in the ceiling and floor.\n"
                (True, _   ) -> "There is a hole in the ceiling.\n"
                (_   , True) -> "There is a hole in the floor.\n"
                _            -> []
              walls = showWalls r

showWalls :: Room -> String
showWalls r = 
          "North wall: "    ++ showJust n
       ++ ".\nSouth wall: " ++ showJust s
       ++ ".\nEast wall: "  ++ showJust e
       ++ ".\nWest wall: "  ++ showJust w ++ ".\n\n"
  where [n, s, e, w] = map (r^.) [wallN, wallS, wallE, wallW]
        showJust Nothing  = "nonexistent"
        showJust (Just a) = show a

showEntities :: Room -> String
showEntities r = concatMap show' $ M.toList (r^.entities)
  where show' (spc, entity) = 
          "There is "  
          ++ show entity 
          ++ " in the " 
          ++ showRelativeDirection (fromVector3 spc) 
          ++ ".\n"

showEntitiesWithIds :: Room -> String
showEntitiesWithIds r = concatMap show' $ M.toList (r^.entities)
  where show' (spc, entity) = 
          show (toInt3 spc) 
          ++ ": " 
          ++ show entity 
          ++ " in the " 
          ++ showRelativeDirection (fromVector3 spc) 
          ++ ".\n"

addEntityAt :: Vector3 Int -> Entity -> Room -> Room
addEntityAt v e = entities %~ (M.insert v e)

getEntityAt :: Vector3 Int -> Room -> Maybe Entity
getEntityAt v r = M.lookup v (r^.entities)

instance Universe RoomType where
  universe = enumFrom NormalRoom
  
-- completely void room
instance Default Room where
  def = Room 
          M.empty     -- | Entities
          NormalRoom  -- | Room Type
          False False -- | Ladders
          Nothing Nothing Nothing Nothing -- | Walls
          False False -- | Holes

-- demolish a wall in some direction
demolish :: Direction -> Room -> Room
demolish d r = r'
  where r' = case d of
                N -> wallN     .~ Nothing $ r
                S -> wallS     .~ Nothing $ r
                E -> wallE     .~ Nothing $ r
                W -> wallW     .~ Nothing $ r
                U -> ceilHole  .~ True    $ r
                D -> floorHole .~ True    $ r
                _ -> r

addLadderU, addLadderD :: Room -> Room
addLadderU r = ladderU .~ True $ r
addLadderD r = ladderD .~ True $ r

-- random rooms are:
-- 100% normal
-- have 4 random walls surrounding them
-- have some number of random entities involved...
-- NB. I want to (maybe...) make this a little more "chunked", as in
--     make it so that the player can find patterns.
randMinesRoom :: MonadRandom m => m Room
randMinesRoom = do
  [n, s, e, w]    <- replicateM 4 $     liftM Just randWall
  [tops, bottoms] <- replicateM 2 $     descending [1..5]
  topEs           <- replicateM tops    randMinesTopEntity
  bottomEs        <- replicateM bottoms randMinesBottomEntity
  topSpaces       <- choose     tops    (map (fromJust . toVector3) topDirs)
  bottomSpaces    <- choose     bottoms (map (fromJust . toVector3) bottomDirs)
  [lu, ld]        <- replicateM 2 $     fromList [(True, 1), (False, 10)]
  let es = (zip topSpaces topEs) ++ (zip bottomSpaces bottomEs)
  return $ entities .~ (M.fromList es)
         $ wallN    .~ n
         $ wallS    .~ s
         $ wallE    .~ e
         $ wallW    .~ w
         $ ladderU  .~ lu
         $ ladderD  .~ ld
         $ def