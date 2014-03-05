{-# LANGUAGE TemplateHaskell #-}
module Types.Vectors(
	Axis(..),
	Vector2(..),
	Vector3(..),
  Vector(..),
  fromTuple,
  fromTriple,
  toInt2,
  fromInt2,
  toInt3,
  fromInt3
) where

import Data.List
import Control.Lens

data Axis = X | Y | Z deriving (Show, Eq)

data Vector2 a = Vector2 a a   deriving (Show, Eq, Ord)
data Vector3 a = Vector3 a a a deriving (Show, Eq, Ord)

makeLenses ''Vector2
makeLenses ''Vector3

-- | NB. toInt2, toInt3 only for use with vectors that can be represented
-- |     in base 3! This is an ad-hoc way for representing space IDs

-- base 3 representation of a Vector2
-- | NB. toInt2 . fromInt2 = id
-- |     fromInt2 . toInt2 = id
toInt2 :: Vector2 Int -> Int
toInt2 (Vector2 x y) = 3 * y + x

toBase3 :: Int -> [Int]
toBase3 0 = [0]
toBase3 1 = [1]
toBase3 2 = [2]
toBase3 x = (x `mod` 3)  : toBase3 (x `div` 3)

fromInt2 :: Int -> Vector2 Int
fromInt2 x = Vector2 x' y'
  where [x', y'] = 
          case toBase3 x of
            [x']    -> [x', 0]
            a@[_,_] -> a
            _       -> error "Cannot convert number larger than 8 to Vector2."

-- base 3 representation of a Vector3
-- x, y, z in 0..2 (though z will typically be 0 or 1)
-- | NB. toInt3 . fromInt3 = id
-- |     fromInt3 . toInt3 = id
toInt3 :: Vector3 Int -> Int
toInt3 (Vector3 x y z) = 9 * z + 3 * y + x

fromInt3 :: Int -> Vector3 Int
fromInt3 x = Vector3 x' y' z'
  where [x', y', z'] =
          case toBase3 x of
            [x']      -> [x', 0, 0]
            [x', y']  -> [x', y', 0]
            a@[_,_,_] -> a
            _         -> error "Cannot convert number larger than 26 to Vector3"

class Vector v where
  vmap :: Axis -> (a -> a) -> v a -> Maybe (v a)
  setV  :: Axis -> a -> v a -> Maybe (v a)
  setV axis x = vmap axis (const x)
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