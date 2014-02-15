-- NB. I would like to use Vector2 for room locations (definitely)
-- and Vector3 for square locations within rooms (maybe)
module Types.Vectors(
	Axis(..),
	Vector2(..),
	Vector3(..),
  Vector(..),
  fromTuple,
  fromTriple,
  toInt2,
  toInt3
) where

data Axis = X | Y | Z deriving (Show, Eq)

data Vector2 a = Vector2 a a   deriving (Show, Eq)
data Vector3 a = Vector3 a a a deriving (Show, Eq)

-- | NB. toInt2, toInt3 only for use with vectors that can be represented in base 3!
-- | This is an ad-hoc way for representing space IDs

-- base 3 representation of a Vector2
toInt2 :: Vector2 Int -> Int
toInt2 (Vector2 x y) = y + 3 * x

-- base 3 representation of a Vector3
-- x, y in 0..2
-- z in 0..1
toInt3 :: Vector3 Int -> Int
toInt3 (Vector3 x y z) = 9 * z + 3 * y + x

class Vector v where
  vmap :: Axis -> (a -> a) -> v a -> Maybe (v a)
  set  :: Axis -> a -> v a -> Maybe (v a)
  set axis x = vmap axis (const x)
  getAxis  :: Axis -> v a -> (Maybe a)
  smult :: (Functor v, Num n) => n -> v n -> v n
  smult n v = fmap (*n) v

instance Vector Vector3 where
  vmap X f (Vector3 x y z) = Just $ Vector3 (f x) y z
  vmap Y f (Vector3 x y z) = Just $ Vector3 x (f y) z
  vmap Z f (Vector3 x y z) = Just $ Vector3 x y (f z)
  getAxis X (Vector3 x y z)    = Just x
  getAxis Y (Vector3 x y z)    = Just y
  getAxis Z (Vector3 x y z)    = Just z

instance Vector Vector2 where
  vmap X f (Vector2 x y) = Just $ Vector2 (f x) y
  vmap Y f (Vector2 x y) = Just $ Vector2 x (f y)
  vmap _ _ _             = Nothing
  getAxis X (Vector2 x y)    = Just x
  getAxis Y (Vector2 x y)    = Just y
  getAxis _ _                = Nothing

instance Functor Vector2 where
  fmap f (Vector2 x y) = Vector2 (f x) (f y)

instance Functor Vector3 where
  fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

fromTuple :: (a, a) -> Vector2 a
fromTuple (x, y) = Vector2 x y

fromTriple :: (a, a, a) -> Vector3 a
fromTriple (x, y, z) = Vector3 x y z