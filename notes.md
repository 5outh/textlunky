# Textlunky Notes

## Things
#### Random generation

* Condition: (mostly) nonexistent
* Next steps:
  * Move random generators to appropriate `Types` modules
    * Can include specialized generators (i.e. type of item depends on game area)
    * Task size: small
  * Make use of a probability distribution monad for more control
    * Task size: medium

#### Game state modification

* Condition: nonexistent
* Next steps:
  * Program enemy AI
    * Task size: large
  * Allow player to interact with environment
    * Task size: medium-large
    * Switch rooms
    * Complete levels

#### Game viewing

* Condition: Partially completed
* Next steps:
  * View adjacent rooms
    * Task size: small
  * Make player items affect ability to view items (spectacles, dark levels, etc)
    * Task size: small
  * (maybe) Add visual component w/ Unicode
    * Task size: medium
    
#### Types

* Condition: Mostly done (for the Mines)
* Next steps:
  * Add more player items
    * Task size: trivial
  * Review what exists already and possibly refactor using a more appropriate design pattern. In particular, I don't really like `Entity`.
    * Task size: ???

#### Game Loop
* Condition: Working
* Next steps:
  * Modularity (prompt, stepGame, interactGame to new modules, etc)
    * Task size: ongoing, will be done in small chunks.
  * Fill in steps (i.e everything mentioned above)

## Daily notes
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
  
## Wed, January 15th

Question: How to differentiate "special" items from other items?
* Can use a dynamic probability distribution to filter out "special" items.
* For things like Hedjet, Udjat Eye, maybe modify player directly when fulfilling necessary actions

## Fri, January 17th

I have changed the GameState to include the Player as an object outside of the internal level.
The reason for this is that before, I would be processing the level in terms of itself(i.e. The game would run even without a player).
Now, a player is *necessary* for the game to even run. Also, it's more intuitive to take two steps:
  
  1. Modify the level based on the player's actions
  2. Modify the player based on the level's movements
 
...instead of performing the single action of modifying the level based on...well, the current state of the level.

This also allows us to take the actions from the `Free` monad and match them up to a *specific object*, not just something
hiding in the declaration of the level. It's still part of the Game State, so we can modify it, but it's a more direct route to say
"Let's modify the current player" than to say "Let's find the player in the level, then modify it using these rules."

We're also insisting that each `GameState` has only *one* player, which is important. If I want to add multiplayer later on, it will be *up to 4-player co-op*,
 and this model should allow for a reasonably easy extension to support such a game type.
 
-----

Got rid of `Generators.hs` and started `Random.Tools` which implements everything I need, but more generally.
Will add more specific generators later, especially ones that take into account a probability distribution for what to spawn.

## Fri, January 24th

I want to move (passive) Items held to itemData :: [(Bool, [Item])] where the items in the `True` list are held, and in the `False` list, not held. This will allow for easier choosing for the RNG.

I'm hitting a wall with the current implementation. I can't show user actions failing and react to them. For example, if I want to pick up something, but nothing is there, I'd like to print out:

"You try to pick something up, but nothing exists."

but with the current implementation I can't do that, because each command is processed on its own after evaluating game state and doesn't have access to it. I may need to move the implementation to a 

```haskell
FreeT TextlunkyCommand (StateT GameState IO) ()
```

in order to get the proper behavior...but that might not work either.

In fact, it may be better to hold off on developing the game until I can get events working properly.

## Sun, January 19th

Some abstractions to implement in the next couple of days:

1. Ladders can go up or down. I want to make this an explicit room quality -- A room can have an up ladder and/or a down ladder, and players will be able to move up or down without any specific tiles.

2. Each room will have 4 walls associated with them. These walls will house items instead of having blocks with items in each room. Bombing a wall might yield an item, and udjat eye / specs will allow users to view "inside walls."

3. (MAYBE) Add `Special` typeclass, a typeclass that allows certain elements of a `Universe` to be specified "Special" so as not to be randomly generated, but deliberately generated, i.e.
  * Special Items (Udjat Eye, Necronomicon, Hedjet, Ankh)
  * Special Walls (Vlad's Amulet, etc)
  * Special Enemies (Vampires, etc)
  * NOTE: This will allow us to say things like:
    * If you are in the mines and the player doesn't have udjat eye, add the udjat eye and gold chest to the list of items to possibly generate
    * If you are in a "Dead are restless" level, add vampires to the list of enemies to generate

## Friday, January 31

Started using `Control.Monad.Random` to handle random values based on probability distributions. It's extremely easy. It's possible to build up complex structures based on smaller random computations. For example, this works (of course it does, because monads!):

```haskell
test :: (RandomGen g) => Rand g String
test = do
  a <- uniform [1, 2, 3]
  b <- uniform "abc"
  return $ b : show a
```

Because this is the case, we can define a bunch of probability distributions for each piece of the game and mold them all together to generate the entire thing...all very easily. Cool.

Note: Can change the above type signature to `test :: MonadRandom m => m String`. This allows ghci prompts to not have to use an StdGen in order to evaluate; you can just type in `test` and it'll spit out a random number. This generality is excellent for testing and also (I'm assuming) allows for more flexibility in the types. Use `MonadRandom` where possible.