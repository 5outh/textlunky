textlunky
=========

An unofficial text adventure version of mossmouth's roguelike platformer, Spelunky.


Design notes are located at [notes.md](https://github.com/5outh/textlunky/blob/master/notes.md) if you'd like to take a look at my design process.

### <font color='red'>Discontinued</font>

As it turns out, this isn't really a fun game. It was enjoyable to build some of its pieces, though. In particular, procedural generation was really interesting and easy, and abstracting away the entire flow of the program into the `Process` modules was also really awesome. I think Haskell is an incredible language for expressing complex game behavior, but this game is too complicated for me to be able to complete, especially given that the final product was likely not to be very fun at all (from my testing). 

In any case, My notes are still up about the design of textlunky, and I think that digging through the source code could be beneficial for learning -- if nothing else, textlunky is a decent example of how to use the `FreeT` monad tranformer and incorporate nondeterministic procedural generation into a process. It's been fun, but I'll be working on other things! Sorry to anyone who was looking forward to the final product!
