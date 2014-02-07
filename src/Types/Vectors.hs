-- NB. I would like to use Vector2 for room locations (definitely)
-- and Vector3 for square locations within rooms (maybe)
module Types.Vectors(
	Axis(..),
	Vector2(..),
	Vector3(..),
  Vector(..)
) where

import Types.Direction

data Axis = X | Y | Z | W deriving (Show, Eq)

data Vector2 a = Vector2 a a   deriving (Show, Eq)
data Vector3 a = Vector3 a a a deriving (Show, Eq)

class Vector v where
  vmap :: Axis -> (a -> a) -> v a -> Maybe (v a)
  set  :: Axis -> a -> v a -> Maybe (v a)
  set axis x = vmap axis (const x)
  get  :: Axis -> v a -> (Maybe a)
  smult :: (Functor v, Num n) => n -> v n -> v n
  smult n v = fmap (*n) v

instance Vector Vector3 where
  vmap X f (Vector3 x y z) = Just $ Vector3 (f x) y z
  vmap Y f (Vector3 x y z) = Just $ Vector3 x (f y) z
  vmap Z f (Vector3 x y z) = Just $ Vector3 x y (f z)
  vmap _ _ _               = Nothing
  get X (Vector3 x y z)    = Just x
  get Y (Vector3 x y z)    = Just y
  get Z (Vector3 x y z)    = Just z
  get _ _                  = Nothing

instance Vector Vector2 where
  vmap X f (Vector2 x y) = Just $ Vector2 (f x) y
  vmap Y f (Vector2 x y) = Just $ Vector2 x (f y)
  vmap _ _ _             = Nothing
  get X (Vector2 x y)    = Just x
  get Y (Vector2 x y)    = Just y
  get _ _                = Nothing

instance Functor Vector2 where
  fmap f (Vector2 x y) = Vector2 (f x) (f y)

instance Functor Vector3 where
  fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)
