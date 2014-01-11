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
  * Entire game operates in a StateT GameState IO ()
  * Can use free monads to represent user movement. With this we can (using only one data type):
    1. Interpret the code and make level modifications
    2. Print the action that was just performed
  * Use Pipes?
  * For random generation, there should be some probability distribution. We don't want *complete* randomness, so there must be something like that. I'll look into it.