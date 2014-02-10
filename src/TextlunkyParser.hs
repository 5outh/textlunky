{-# LANGUAGE TupleSections #-}
module TextlunkyParser(
  parseInput
)

where

import Data.List.Split
import Control.Applicative hiding((<|>))
import Types.TextlunkyCommand
import Types.Direction
import Types.Synonyms
import qualified Data.Map as M
import Control.Monad.Trans.Free
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Applicative
import Data.Maybe
import Data.List

-- NB. check BINARY commands first, then unary ones.

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

-- | This is all that really matters to the rest of the application
parseInput :: String -> Textlunky ()
parseInput = parseCommands . getCommands

getCommands :: String -> [[String]]
getCommands = splitOneOf combine . splitOn " "

parseUnary :: [String]                    -- Possible Command List
           -> (() -> TextlunkyCommand ()) -- Unary Command
           -> [String]                    -- User Command List
           -> Maybe (Textlunky ())        -- Response
parseUnary cmdList cmd xs = 
  case any (`elem` cmdList) xs of
    True -> Just $ liftF $ cmd ()
    _    -> Nothing

parseDirectionBinary :: [String] 
                     -> (Direction -> () -> TextlunkyCommand ()) 
                     -> [String] 
                     -> Maybe (Textlunky ())
parseDirectionBinary cmdList cmd xs =
    case any (`elem` cmdList) xs of
      True -> case parseDirection xs of
                Just dir -> Just $ liftF $ cmd dir ()
                _        -> Nothing
      _    -> Nothing

-- find the first direction mentioned in a command string
parseDirection :: [String] -> Maybe Direction
parseDirection xs = 
  case filter isJust dirs of
        []    -> Nothing
        (x:_) -> x
  where dirs = map (flip M.lookup directionMap) xs


{- ************ START PARSERS ************** -}

-- 0-ary parser
parseEnd xs = if any (`elem` end) xs
              then Just (liftF End) 
              else Nothing

-- Unary parsers
parseDropItem     = parseUnary dropItem DropItem
parsePickupU      = parseUnary pickup   (Pickup  Nothing)
parseJumpU        = parseUnary jump     (Jump    Nothing)
parseAttackU      = parseUnary attack   (Attack  Nothing)
parseRope         = parseUnary rope     Rope
parseExitLevel    = parseUnary leave    ExitLevel
parseShowRoom     = parseUnary showRoom ShowFull
parseShowEnts     = parseUnary showEnts ShowEntities
parseWalls        = parseUnary showWall Walls

-- kill self if user doesn't specify what to shoot
parseShootSelf xs =   parseUnary shoot     ShootSelf xs
                  <|> parseUnary attack    ShootSelf xs
                  <|> parseUnary shootself ShootSelf xs

parseDropDown xs = case "drop" `elem` xs of 
  True -> case "down" `elem` xs of 
            True -> Just $ liftF $ DropDown ()
            _    -> Nothing
  _    -> case any (`elem` dropDown) xs of
            True -> Just $ liftF $ DropDown ()
            _    -> Nothing
  where dropDown = ["fall", "godown"]

-- check for desire to open gold chest 
parseOpenChest  xs = case any (`elem` open) xs of 
  True -> case "gold" `elem` xs of
            True -> Just $ liftF $ OpenGoldChest ()
            _    -> Just $ liftF $ OpenChest ()
  _    -> Nothing


-- Binary (Direction) Parsers
-- TODO: Enemy and Entity parsers
parseMoveB     =   parseDirectionBinary move   Move
parseShootB xs =   parseDirectionBinary shoot  ShootD xs
               <|> parseDirectionBinary attack ShootD xs
parseLook      =   parseDirectionBinary look   Look
parseThrow     =   parseDirectionBinary throw  Throw
parseBombB  xs = case any (== "bomb") xs of
  True -> Just $ liftF $ Bomb (parseDirection xs) ()
  _    -> Nothing

parseCommands :: [[String]] -> Textlunky ()
parseCommands []     = return ()
parseCommands (x:xs) = 
  case parseCommand x of
        Just f -> do f
                     parseCommands xs
        _      -> parseCommands xs

--TODO: Parse Enemy and Entity actions
parseCommand :: [String] -> Maybe (Textlunky ())
parseCommand xs = foldr1 (<|>) 
                . map ($ xs)
                $ [ parseMoveB    , 
                    parseShootB   , 
                    parseLook     ,
                    parseDropDown , 
                    parseThrow    , 
                    parseBombB    , 
                    parseDropItem ,
                    parseShowRoom , 
                    parsePickupU  , 
                    parseJumpU    , 
                    parseAttackU  , 
                    parseShowEnts , 
                    parseWalls    ,
                    parseRope     , 
                    parseShootSelf, 
                    parseExitLevel,
                    parseOpenChest, 
                    parseEnd       ]

{- ************ END PARSERS ************** -}

{- ************ START COMMAND LISTS ************** -}

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
look      = ["look", "view", "peek", "search", "examine"]
combine   = ["&", "and", "then", "."] -- | Process many commands at once
end       = ["end", "quit"]           -- | For completeness
showWall  = ["walls", "lookwalls"]
showEnts  = ["stuff", "entities", "contents"]
showRoom  = ["show", "search", "room", "look", "view", "examine"]

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


{- ************ END COMMAND LISTS ************** -}

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
                    ((,liftF (DropItem ()))         , dropItem  ),
                    ((,liftF (Pickup Nothing ()))   , pickup    ),
                    ((,liftF (Jump Nothing ()))     , jump      ),
                    ((,liftF (Attack Nothing ()))   , attack    ) ]