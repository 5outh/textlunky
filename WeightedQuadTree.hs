module WeightedQuadTree(
  WeightedQuadTree(..),
  Direction(..),
  getVal,
  setVal,
  move,
  setEdge,
  getEdge,
  modifyVal,
  modifyEdge,
  --for testing
  example
  )
where 

import Types(Direction(..))

-- | w is the weight of each edge
data WeightedQuadTree w a = 
  WQTree a 
    ( (WeightedQuadTree w a), w )
    ( (WeightedQuadTree w a), w )
    ( (WeightedQuadTree w a), w )
    ( (WeightedQuadTree w a), w )
  | Leaf
  deriving (Show, Eq)

instance Show Direction where 
  show U = "up"
  show D = "down"
  show L = "left"
  show R = "right"

example :: WeightedQuadTree (Maybe Char) Int 
example = WQTree 0 (Leaf, Nothing) (Leaf, Nothing) (Leaf, Nothing) (Leaf, Nothing)

getVal :: WeightedQuadTree w a -> a
getVal (WQTree a _ _ _ _) = a
getVal Leaf = error "Attempt to get value of leaf node."

setVal :: WeightedQuadTree w a -> a -> WeightedQuadTree w a
setVal (WQTree _ l r u d) x = WQTree x l r u d

move :: Direction -> WeightedQuadTree w a -> WeightedQuadTree w a
move dir (WQTree _ l r u d) = fst $ case dir of
  L -> l
  R -> r
  U -> u 
  D -> d

setEdge :: Direction -> WeightedQuadTree w a ->  w -> WeightedQuadTree w a
setEdge dir (WQTree x (l, a) (r, b) (u, c) (d, e)) w = case dir of
  L -> WQTree x (l, w) (r, b) (u, c) (d, e)
  R -> WQTree x (l, a) (r, w) (u, c) (d, e)
  U -> WQTree x (l, a) (r, b) (u, w) (d, e)
  D -> WQTree x (l, a) (r, b) (u, c) (d, w)
setEdge _ Leaf _ = error "Attempt to modify edge value of a leaf node."

getEdge :: Direction -> WeightedQuadTree w a -> w
getEdge dir (WQTree _ l r u d) = snd $ case dir of 
  L -> l
  R -> r
  U -> u
  D -> d

modifyVal :: WeightedQuadTree w a -> (a -> a) -> WeightedQuadTree w a
modifyVal tree f = setVal tree (f $ getVal tree)

modifyEdge :: Direction -> WeightedQuadTree w a -> (w -> w) -> WeightedQuadTree w a
modifyEdge dir tree f = setEdge dir tree (f $ getEdge dir tree)