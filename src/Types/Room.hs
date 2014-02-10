{-# LANGUAGE TemplateHaskell #-}
module Types.Room(
  Room(..),
  RoomType(..),
  randMinesRoom,
  demolish,
  showWalls,
  showEntities
) where

import Data.Default
import Data.Universe
import Data.List(intercalate)
import Types.Direction
import Types.Entity
import Types.Item
import Types.Wall
import Types.Vectors
import Control.Lens
import Control.Monad(replicateM, liftM)
import Random.Probability

data RoomType = NormalRoom
              | KaliAltar
              | Shop
              | LevelExit
                deriving (Enum, Eq)


data Room = Room{
    _entities :: [(Vector3 Int, Entity)],
    _rType    :: RoomType,
    -- | See 1/19 notes
    -- | Define like this for absolute correctness,
    -- | Don't allow for an Up wall, for example
    _ladderU   :: Bool      ,
    _ladderD   :: Bool      ,
    _wallN     :: Maybe Wall,
    _wallS     :: Maybe Wall,
    _wallE     :: Maybe Wall,
    _wallW     :: Maybe Wall,
    _ceilHole  :: Bool      ,
    _floorHole :: Bool      
}

makeLenses ''Room

-- full room show
instance Show Room where
    show r = concat $ (walls : ladders : holes : t : map show' (r^.entities))
        where show' (spc, entity) = "There is "  ++ show entity ++ " in the " ++ showRelativeDirection (fromVector3 spc) ++ ".\n"
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
showEntities r = concatMap show' (r^.entities)
  where show' (spc, entity) = "There is "  ++ show entity ++ " in the " ++ showRelativeDirection (fromVector3 spc) ++ ".\n"

instance Universe RoomType where
  universe = enumFrom NormalRoom
  
-- completely void room
instance Default Room where
  def = Room 
          []          -- | Entities
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

-- random rooms are:
-- 100% normal
-- have 4 random walls surrounding them
-- have some number of random entities involved...
-- NB. I want to (maybe...) make this a little more "chunked", as in
--     make it so that the player can find patterns.
randMinesRoom :: MonadRandom m => m Room
randMinesRoom = do
  [n, s, e, w]    <- replicateM 4 $     liftM Just randWall
  [tops, bottoms] <- replicateM 2 $     descending [1..6]
  topEs           <- replicateM tops    randMinesTopEntity
  bottomEs        <- replicateM bottoms randMinesBottomEntity
  topSpaces       <- choose     tops    (map toVector3 topDirs)
  bottomSpaces    <- choose     bottoms (map toVector3 bottomDirs)
  [lu, ld]        <- replicateM 2 $     fromList [(True, 1), (False, 10)]
  let es = (zip topSpaces topEs) ++ (zip bottomSpaces bottomEs)
  return $ entities .~ es
         $ wallN    .~ n
         $ wallS    .~ s
         $ wallE    .~ e
         $ wallW    .~ w
         $ ladderU  .~ lu
         $ ladderD  .~ ld
         $ def

