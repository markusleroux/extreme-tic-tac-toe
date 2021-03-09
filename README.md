# Extreme Tic-Tac-Toe

When I started with Haskell in February, the first thing that I built was a tic tac toe game. Unfortunately, I accidentally deleted the file before I uploaded it to GitHub. Now that I've learnt a little more about the language, I thought I would return and build a
new version. To add to the fun, I've built a nested version of the game.

## Rules

The rules are quite simple. You play like regular tic-tac-toe, except that the sub-board you play in is dictacted by the square
chosen by the other player in the previous round. For example, if Player X is playing on sub-board (1, 3) and chooses to capture
square (1, 1), then Player O must select a square in sub-board (1, 1) to capture in the next round.

## Installation

The game is set up as a Stack script, which means that it can be run from the directory hosting the file with

``` sh
stack game.hs
```
