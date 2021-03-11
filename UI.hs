#!/usr/bin/env stack
-- stack --resolver lts-17.4 script

module UI where

import TicTacToe

import Brick

import qualified Graphics.Vty as V

type Name = ()

app :: App GameState () Name
app =
  App { appDraw = drawUI
      , appChooseCursor = neverShowCursor
      , appHandleEvent = handleEvent
      , appStartEvent = return
      , appAttrMap = const theMap
      }

main :: IO ()
main = undefined

handleEvent :: GameState -> BrickEvent Name () -> EventM Name ( Next GameState )
handleEvent gs (VtyEvent (V.EvKey V.KUp []))         = continue $ moveCursor Up gs
handleEvent gs (VtyEvent (V.EvKey V.KDown []))       = continue $ moveCursor Down gs
handleEvent gs (VtyEvent (V.EvKey V.KRight []))      = continue $ moveCursor Right gs
handleEvent gs (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveCursor Left gs
handleEvent gs (VtyEvent (V.EvKey V.KEnter []))      = continue $ playSquare gs
handleEvent gs (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt gs
handleEvent gs (VtyEvent (V.EvKey V.KEsc []))        = halt gs
handleEvent gs _                                     = continue gs

drawUI :: GameState -> [Widget Name]
drawUI = undefined

theMap :: AttrMap
theMap = undefined
