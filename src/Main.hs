module Main where

import UI
import Game
import Brick (defaultMain)
import Lens.Micro.GHC ((^.))

main :: IO ()
main = do
  finalState <- defaultMain app initialState
  putStrLn $ displayBoard $ finalState ^. board
  putStrLn $ show $ finalState ^. meta
  
