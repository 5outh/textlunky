#### Textlunky Notes

### Things
# Random generation

* Condition: nonexistent

# Game state modification

* Condition: nonexistent

# Game viewing

* Condition: Partially completed

# Types

* Condition: Mostly done (for the Mines)

# Game Loop
* Works!
 
### Pipeline

1. Generate random floor
  1. Generate 9 rooms (logistics of this in the works)
  2. Make a random walk from the entrance on the top level down to the exit on the bottom level, opening sides of rooms as we pass through them to ensure player has a way to escape the current level.
  3. Load the player from the GameState & place at entrance
2. Print current room description
3. Ask for user input
4. Respond to user input
  1. Modify Level
  2. Print action
5. Step each entity one round in the game
6. Repeat steps 2..5 until player reaches exit or dies
  1. If player reaches exit, step level number and possibly area and repeat from step 1.
  
Things to worry about:
  
  * Random generation (global seed generated in `main`)
  * How to modify pieces of Game state without modifying all of it (`Lenses`)
  * How to keep a global state in tact
  
Ideas:
  * Entire game operates in a `StateT GameState IO ()`
  * Can use free monads to represent user movement. With this we can (using only one data type):
    1. Interpret the code and make level modifications
    2. Print the action that was just performed
  * Use Pipes?
  * For random generation, there should be some probability distribution. We don't want *complete* randomness, so there must be something like that. I'll look into it.

What happens when the player performs an action, on the entire game state?
  * Player location might move
  * Player needs to interact with elements in the current room
  * GameState's room might change
  * GameState's level & level number might change
  * GameState's area might change
  * **All of these things need to be taken into account when performing a player action**

What happens when the game moves one tick?
  * Enemies can move and attack, potentially into the same spots (which is okay, actually)
  * Tiles may fall
  * **Actually, not that much. Mainly enemy movement.**
  
Psuedocode:

```haskell
main = do
  seed <- genRandomSeed :: StdGen            -- get a global standard generator                  
  startState <- getStartGameState seed       -- generate the starting game state (randomness necessary for random initial level)
  flip runStateT startState $ gameLoop seed  -- Run the game, using the start state and seed

gameLoop :: StdGen -> StateT GameState IO ()
gameLoop gen = forever $ do
  cmd <- lift getPlayerCommand                   -- get a player command
  modify $ 
  	(processCmd 
  	 $ (liftF . parse) cmd :: Free CommandF () ) -- update full state based on player move
  zoom level $ modify updateLevel                -- update level separately
  modify updateState                             -- update full state
```

## Mon, January 13th

Need:
1. A parser for text commands (parsec)
2. An interpreter for text commands 
3. A printer for text commands
4. A pipeline for the actual program (maybe the last step)

Note: 2 & 3 can be sort of tackled together if we use a free monad, so that's something I want to do.

I still need to define the instruction set provided for the user. Just brainstorming...
Note: `_` denotes empty arguments

* move [direction]
	* move udlr
* moveto [item]
	* move to item on board if it exists and is reachable
* pickup [item | _]
	* move to location of `item` and pick it up
	* default behavior: pick up item in same spot as player
* jump [enemy | _]
	* jump on enemy
	* default behavior: jump in place
* attack [enemy | _]
	* attack enemy and move to its location
	* default behavior : whip in current location
	* NB. if player is holding something, use its attack instead (shotgun etc)
* shoot [direction | enemy | _]
	* direction: fire weapon in said direction
	* enemy: fire at specific enemy
	* default: shoot self, instant death
	* NB. if player isn't holding a gun, does nothing. 
* throw [direction]
	* throw held item in any direction
	* if held item doesn't exist, do nothing & print message
* rope
	* throw rope up
* climb [rope | ladder]
	* climb rope or ladder
	* default behaviour: climb rope or ladder if exists and is unambiguous
* bomb [direction | _]
	* bomb a wall in some direction & move to that location
	* default behavior: bomb current location
	* Goes off one round later
* open chest
	* if holding key, open gold chest
	* if not, open any other chest in room
* <cmd1> & <cmd2> & ... & <cmdn>
	* Perform all commands on the same round, in succession.
	* Cannot move from room more than once
	* May impose a maximum on concurrent commands
* exit
	* move to exit square and exit the level
	* NB. Only works if current room is exit room
* drop
	* if exit below exists, fall down to next level
	* if not, do nothing
* look [direction]
	* if a room exists in direction, print out its description.