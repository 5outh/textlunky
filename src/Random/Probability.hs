module Random.Probability(
  ascending,
  descending,
  permutation,
  choose,
  withWeight,
  fromUniverse,
  module Control.Monad.Random
) where

import Data.Ratio
import Control.Monad.State
import Control.Monad.Random
import Data.Universe
import Data.List(delete)

-- Things at beginning of list have lower weight
ascending :: MonadRandom m => [a] -> m a
ascending = fromList . flip zip [1..]

-- Things at beginning of list have higher weight
descending :: MonadRandom m => [a] -> m a
descending xs = fromList $ zip xs [l, (l-1)..0]
  where l = fromIntegral $ length xs

withWeight :: Rational -> [a] -> [(a, Rational)]
withWeight n = fmap (flip (,) n)

fromUniverse :: (MonadRandom m, Universe a) => m a
fromUniverse = uniform universe

-- Permutations and Choose are list-based for now.
-- At some point, they might end up being MonadRandom based,
-- but right now I'm not sure how that will work.
permutation :: (Eq a, MonadRandom m) => [a] -> m [a]
permutation = weightedPermutation uniform

weightedPermutation :: (Eq a, MonadRandom m) => ([a] -> m a) -> [a] -> m [a]
weightedPermutation _ [] = return []
weightedPermutation f xs = do
  x <- f xs
  p <- weightedPermutation f (delete x xs)
  return $ x : p

choose :: (Eq a, MonadRandom m, Integral n) => n -> [a] -> m [a]
choose = weightedChoose uniform

weightedChoose :: 
  (Eq a, MonadRandom m, Integral n) => ([a] -> m a) -> n -> [a] -> m [a]
weightedChoose _ 0 _  = return []
weightedChoose f n xs = do
  x <- f xs
  c <- weightedChoose f (n-1) (delete x xs)
  return $ x : c