module TextlunkyParser(
)

where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding((<|>))
import Types.TextlunkyCommand

-- | Parse a string into a TextlunkyCommand


-- | Available initial Commands
commands :: [String]
commands = 
  ["move", "m", "walk", "go", "mv",          -- | Move
   "move to", "go to", "mvto", "goto",       -- | MoveTo
   "pickup", "take", "grab",                 -- | Pickup
   "drop", "put down",                       -- | DropItem
   "jump", "jmp", "leap",                    -- | Jump
   "attack", "kill", "murder", "destroy",    -- | Attack
   "shoot", "fire",                          -- | ShootD, ShootE, ShootSelf
   "throw", "chuck", "toss",   
   "rope", "throwrope",                      -- | Rope
   "bomb",                                   -- | Bomb
   "open", "chest", "openchest", "getchest", -- | OpenChest, OpenGoldChest
   "leave", "exit", "finish", "end",         -- | ExitLevel
   "drop", "dropdown", "fall", "hole",       -- | DropDown
   "look", "view", "peek", "search"]         -- | Look
                                             -- | NB. "End" implicit

directions :: [String]
directions = 
  ["n", "north", "forward", "fw",          -- | North
   "s", "south", "backwards", "back", "b", -- | South
   "e", "east", "right", "r",              -- | East
   "w", "west", "left", "l",               -- | West
   "d", "down",                            -- | Down
   "u", "up"                               -- | Up
  ]