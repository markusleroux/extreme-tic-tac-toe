module UI where

import Game

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

handleEvent :: GameState -> BrickEvent Name () -> EventM Name ( Next GameState )
handleEvent gs (VtyEvent (V.EvKey V.KUp []))         = continue $ moveCursor North gs
handleEvent gs (VtyEvent (V.EvKey V.KDown []))       = continue $ moveCursor South gs
handleEvent gs (VtyEvent (V.EvKey V.KRight []))      = continue $ moveCursor East gs
handleEvent gs (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveCursor West gs
handleEvent gs (VtyEvent (V.EvKey V.KEnter []))      = continue $ playSquare gs
handleEvent gs (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt gs
handleEvent gs (VtyEvent (V.EvKey V.KEsc []))        = halt gs
handleEvent gs _                                     = continue gs

drawUI :: GameState -> [Widget Name]
drawUI = undefined

theMap :: AttrMap
theMap = undefined
