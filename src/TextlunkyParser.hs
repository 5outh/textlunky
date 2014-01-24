{-# LANGUAGE TupleSections #-}
module TextlunkyParser(
)

where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding((<|>))
import Types.TextlunkyCommand
import Types.Direction
import qualified Data.Map as M
import Control.Monad.Trans.Free
import Control.Monad.Identity

-- | Parse a string into a TextlunkyCommand

-- | - Bomb
-- | 
-- | Binary: 
-- | - Move     - Direction
-- | - Look     - Direction
-- | - Throw    - Direction
-- | - ShootD   - Direction
-- | - MoveTo   - Entity
-- | - Pickup   - (Maybe Entity)
-- | - DropItem - (Maybe Entity)
-- | - Jump     - (Maybe Enemy)
-- | - Attack   - (Maybe Enemy)
-- | - ShootE   - Enemy
-- | - Bomb     - (Maybe Direction)

move      = ["move", "m", "walk", "go", "mv"]
moveTo    = ["move to", "go to", "mvto", "goto"]
pickup    = ["pickup", "take", "grab"]
dropItem  = ["drop", "put down"]
jump      = ["jump", "jmp", "leap"]
attack    = ["attack", "kill", "murder", "destroy"]
shoot     = ["shoot", "fire"]
shootself = ["die", "killself"]
throw     = ["throw", "chuck", "toss"]
rope      = ["rope", "throwrope"]
bomb      = ["bomb"]
open      = ["open", "chest", "openchest", "getchest"]
leave     = ["leave", "exit", "finish", "end"]
dropDown  = ["drop", "dropdown", "fall", "hole"]
look      = ["look", "view", "peek", "search"]
combine   = ["&", "and", "then"] -- | Process many commands at once
end       = ["end"] -- | For completeness

north  = ["n", "north", "forward", "fw"]
south  = ["s", "south", "backwards", "back", "bk"]
east   = ["e", "east", "right", "r"]
west   = ["w", "west", "left", "l"]
up     = ["u", "up"]
down   = ["d", "down"]
middle = ["middle", "mid", "m"]

-- | Available initial Commands
commands :: [String]
commands = concat [move, moveTo, pickup, dropItem, jump, attack, shoot, 
                   throw, rope, bomb, open, leave, dropDown, look, end]

-- | Corresponds to Direction
directions :: [String]
directions = concat [north, south, east, west, up, down, middle]

directionMap :: M.Map String Direction
directionMap = M.fromList $ dirs >>= (uncurry map)
  where dirs = [ ((,N), north ),
                 ((,S), south ),
                 ((,E), east  ),
                 ((,W), west  ),
                 ((,U), up    ),
                 ((,D), down  ),
                 ((,M), middle) ]

-- | Unary: 
-- | - ShootSelf
-- | - Rope
-- | - OpenGoldChest
-- | - OpenChest
-- | - ExitLevel
-- | - DropDown
-- | - End (?)
-- | (with Nothing)
-- | - DropItem
-- | - Pickup
-- | - DropItem
-- | - Jump
-- | - Attack

-- | Associates unary operations with their corresponding free action
unaryMap :: M.Map String (FreeT TextlunkyCommand Identity ())
unaryMap = M.fromList $ unaries >>= (uncurry map)
  where unaries = [ ((,liftF (ShootSelf ()))        , shootself ),
                    ((,liftF (Rope ()))             , rope      ),
                    ((,liftF (OpenChest ()))        , open      ),
                    ((,liftF (ExitLevel ()))        , leave     ),
                    ((,liftF (DropDown ()))         , dropDown  ),
                    ((,liftF (DropItem Nothing ())) , dropItem  ),
                    ((,liftF (Pickup Nothing ()))   , pickup    ),
                    ((,liftF (Jump Nothing ()))     , jump      ),
                    ((,liftF (Attack Nothing ()))   , attack    ) ]

