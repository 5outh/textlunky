module Control.AI(
) where

import Data.Vectors
import Control.Monad.Free

type AI cmd = Free cmd ()