{-  
    Here we're basically just boxing up the Random monad and will add bits to it as necessary.
    The coin example is included below for reference. 

    The idea will be to have probability distributions for each item in the game, and get random 
    values out as necessary during the game. 
-}
module Random.Probability(
  stRand,
  evalStRand,
  runStRand,
  execStRand,
  uniform,
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

-- | run a stateful random computation
-- | note: I think all of this crap is unnecessary
stRand :: (RandomGen g) => Rand g Coin -> State g Coin
stRand rand = do
  g <- get
  let (a, g') = runRand rand g
  put g'
  return a

evalStRand :: (RandomGen g) => g -> Rand g Coin -> Coin
evalStRand g rand = evalState (stRand rand) g

runStRand :: (RandomGen g) => g -> Rand g Coin -> (Coin, g)
runStRand g rand = runState (stRand rand) g

execStRand :: (RandomGen g) => g -> Rand g Coin -> g
execStRand g rand = execState (stRand rand) g

-- | This crap is useful, though.
uniform :: (MonadRandom m) => [a] -> m a
uniform = fromList . fmap (flip (,) 1)

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

weightedChoose :: (Eq a, MonadRandom m, Integral n) => ([a] -> m a) -> n -> [a] -> m [a]
weightedChoose _ 0 _  = return []
weightedChoose f n xs = do
  x <- f xs
  c <- weightedChoose f (n-1) (delete x xs)
  return $ x : c

-- | Coin example
data Coin = Heads | Tails deriving (Show, Eq)

coinR :: (MonadRandom m) => m Coin
coinR = fromList [(Heads, 1), (Tails, 1)]

coinsR :: (MonadRandom m) => Int -> m [Coin]
coinsR n = replicateM n coinR

runCoins :: (RandomGen g) => Int -> State g [Coin]
runCoins n = replicateM n (stRand coinR)