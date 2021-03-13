module Main where

import UI
import Game
import Brick (defaultMain)
import Data.Array (array)
import Lens.Micro ((^.))

main :: IO ()
main = do
  let initialState =
        GS { _board = emptyBoard
           , _meta = emptySubBoard
           , _player = X
           , _ppos = ( 1, 1 )
           , _cursor = ( 1, 1 )
           , _finished = Nothing
           }
  finalState <- defaultMain app initialState
  putStrLn $ displayBoard $ finalState ^. board
  putStrLn $ show $ finalState ^. meta
  where
    emptySubBoard :: SubBoard
    emptySubBoard = array ((1, 1), (3, 3)) [ ( ( x, y ), Nothing ) | x <- [1..3], y <- [1..3] ]

    emptyBoard :: Board
    emptyBoard = array ((1, 1), (3, 3)) [ ( ( x, y ), emptySubBoard ) | x <- [1..3], y <- [1..3] ]
