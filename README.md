# Extreme Tic-Tac-Toe

When I started with Haskell, the first thing that I built was a tic tac toe game. Unfortunately, I accidentally deleted the file before I uploaded it to GitHub. Now that I've learnt a little more about the language, I thought I would return and build a new version. To add to the fun, I've built a nested version of the game.

## Rules

The rules are quite simple. You play like regular tic-tac-toe, except that the sub-board you play in is dictacted by the square chosen by the other player in the previous round. For example, if Player X is playing on sub-board (1, 3) and chooses to capture square (1, 1), then Player O must select a square in sub-board (1, 1) to capture in the next round.

## Installation

The game is set up as a Stack script, which means that it can be run from the directory hosting the file with

``` sh
stack game.hs
```

## Review

I'm going to use this space to record a few thoughts I had while writing this project, since I don't at the moment have a blog and I do think recording what I have learnt will be useful to me, and maybe also to anyone else who reads this.

One of the things that I've found the most fascinating about writing Haskell has been the way that Haskell handles the distinction between the structure of a program and the actual computations that need to be carried out in order to produce the result. This is a bit of tricky concept for me to explain with my current depth of understanding of the language, but it is somewhat similar to the seperation of C programs into headers and sources files. In Haskell, it is often the case that we can distinguish between the code used to set up the computations we wish to make and the computations themselves. In the state monad, for example, we use functions `a -> State a s` to change the monadic computation we wish to perform on the fly. If we view the monadic value as a computational pipeline (when we run the monad, we get a function `s -> (a, s)`), then the function `a -> State a s` can be seen as dynamically changing the computation we wish to run based on the returned value (and not the state) of the previous computation. What we arrive at it is an elegant seperation of the reasoning which determines the computation to run, and the code which is responsible for the computation itself.

In this respository, we can see this seperation in action as the distinction between updating the state based on some IO (the pipeline) and choosing the subboard within which to play (the dynamic adjustments to the pipeline). I've taken things slightly further and written my higher order adjustments as functions which also take the current move as a first parameter. The alternative, of course, would be to include the previous player in the GameState data, but this would prevent me from controlling the order of plays at a high level (at the moment, I can simply rewrite the array I am using to determine who plays when, which is useful for runnng tests and may be valuable if I ever want to change the game). The other major adjustment that I've made is that I've included short circuiting behaviour in the returned value from the state monad. Specifically, return values are Either Move Position; values of the type Position cause the pipeline to change as expected, while values of the type Move cause a short circuit, skipping all remaining computations and congratulating the winner.

One of the notes that I have for myself from my last few projects is that the cost of defining a data type to handle the state, both in terms of performance and in terms of needing to handle an extra layer of indirection, is probably outweighed by the flexibility of being able to add new fields with only minimal changes to the code and signatures. I'm also still getting used to the heavy use of higher order functions and monad stacks, and think that at the moment one of the biggest improvements I could make to my Haskell code is in taking better advantage of distinctions like the one I described above to write code that is not only functional and concise but also cleaner and easier to understand than what I would be able to do with first order functions. Doing so will, I think, come down to ensuring that I use only the most minimal stack of monads when writing each function and ensuring that I choose the most natural intermediary results when conceptualizing my programs.
