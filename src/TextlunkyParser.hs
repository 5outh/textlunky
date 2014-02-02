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

{-| The idea here should be to iterate over a list of user-provided commands, and search for keywords among them.
    When we find keywords that can craft a command (listed below), parse them into the appropriate command.
-}

-- | Parse a string into a TextlunkyCommand

-- | - Bomb
-- | 
-- | Binary: 
-- | - Move     - Direction - eg. "move north"
-- | - Look     - Direction - eg. "look left" "look north" 
-- | - Throw    - Direction - eg. "throw north" "throw up"
-- | - ShootD   - Direction - eg. "shoot right" "shoot north"
-- | - MoveTo   - Entity    - eg. "moveTo shotgun" "moveTo spike pit"
-- | - Pickup   - (Maybe Entity) - eg. "pickup shotgun" "pickup" "pickup damsel"
-- | - DropItem - (Maybe Entity) - eg. "drop" "drop gun" "drop rock"
-- | - Jump     - (Maybe Enemy)  - eg. "jump" "jump on spider" "jump over spike pit" (maybe)
-- | - Attack   - (Maybe Enemy)  - eg. "attack" "attack spider" "kill snake"
-- | - ShootE   - Enemy          - eg. "shoot" "shoot cobra"
-- | - Bomb     - (Maybe Direction) - eg. "bomb" "place bomb" "bomb left wall"

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
-- | - ShootSelf - eg. "kill" "shoot" with no other keywords
-- | - Rope      - eg. "rope" "throw rope" (note "rope" takes precedence over "throw")
-- | - OpenGoldChest - eg. "open gold chest" "open chest" (if no other chests in room)
-- | - OpenChest     - eg. "open chest"
-- | - ExitLevel     - eg. "exit" "leave" "finish" "go through door"
-- | - DropDown      - eg. "fall" "go down" "drop down"
-- | - End (?)       - only for continuity
-- | (with Nothing)  - (no keywords to indicate otherwise)
-- | - DropItem      - eg. "drop <item>" "drop"
-- | - Pickup        - eg. "pick up" "take"
-- | - Jump          - eg. "jump"
-- | - Attack        - eg. "attack"

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

