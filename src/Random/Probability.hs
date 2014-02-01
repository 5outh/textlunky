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
  withWeight,
  fromUniverse,
  module Control.Monad.Random
) where

import Data.Ratio
import Control.Monad.State
import Control.Monad.Random
import Data.Universe

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

withWeight :: Rational -> [a] -> [(a, Rational)]
withWeight n = fmap (flip (,) n)

fromUniverse :: (MonadRandom m, Universe a) => m a
fromUniverse = uniform universe

-- | Coin example
data Coin = Heads | Tails deriving (Show, Eq)

coinR :: (MonadRandom m) => m Coin
coinR = fromList [(Heads, 1), (Tails, 1)]

coinsR :: (MonadRandom m) => Int -> m [Coin]
coinsR n = replicateM n coinR

runCoins :: (RandomGen g) => Int -> State g [Coin]
runCoins n = replicateM n (stRand coinR)