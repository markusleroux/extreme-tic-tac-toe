{-# LANGUAGE OverloadedStrings #-}

module UI where

import Game

import Brick
import Brick.Widgets.Table

import Data.Array ((!))
import Lens.Micro ((^.))

import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

----------------------

type Name = ()

app :: App GameState () Name
app =
  App { appDraw = drawUI
      , appChooseCursor = neverShowCursor
      , appHandleEvent = handleEvent
      , appStartEvent = return
      , appAttrMap = const theMap
      }

----------------------

handleEvent :: GameState -> BrickEvent Name () -> EventM Name ( Next GameState )
handleEvent gs (VtyEvent (V.EvKey V.KUp []))         = continue $ moveCursor North gs
handleEvent gs (VtyEvent (V.EvKey V.KDown []))       = continue $ moveCursor South gs
handleEvent gs (VtyEvent (V.EvKey V.KRight []))      = continue $ moveCursor East gs
handleEvent gs (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveCursor West gs
handleEvent gs (VtyEvent (V.EvKey V.KEnter []))      = continue $ playSquare gs
handleEvent gs (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt gs
handleEvent gs (VtyEvent (V.EvKey V.KEsc []))        = halt gs
handleEvent gs _                                     = continue gs

----------------------

drawUI :: GameState -> [ Widget Name ]
drawUI = return . bw

bw :: GameState -> Widget Name
bw gs = C.centerLayer $ renderTable
  $ surroundingBorder False
  $ setDefaultRowAlignment AlignMiddle
  $ setDefaultColAlignment AlignCenter
  $ table rowsOfSB
  where
    rowsOfSB :: [ [ Widget Name ]]
    rowsOfSB = [ [ getSBW x y | x <- [1..3] ] | y <- [1..3] ]

    getSBW :: Int -> Int -> Widget Name
    getSBW x y = let ( fx, fy ) = gs ^. cursor
                     isFocus = ( x, y ) == ( gs ^. ppos )
                     w = sbw fx fy isFocus $ ( gs ^. board ) ! ( x, y ) in
      if isFocus
         then withAttr fSBAttr w
         else withAttr uSBAttr w


sbw :: Int -> Int -> Bool -> SubBoard -> Widget Name
sbw fx fy isFocus sb = renderTable
  $ surroundingBorder False
  $ setDefaultRowAlignment AlignMiddle
  $ setDefaultColAlignment AlignCenter
  $ table rows
  where
    rows :: [ [ Widget Name ] ]
    rows = [ [ cw ( sb ! ( x, y ) ) | x <- [1..3] ] | y <- [1..3] ]

    getCW :: Int -> Int -> Widget Name
    getCW x y = let w = cw $ sb ! ( x, y ) in
      if isFocus && ( x, y ) == ( fx, fx )
         then withAttr fCAttr w
         else withAttr uCAttr w

    cw :: Cell -> Widget Name
    cw = str . displayCell

----------------------

fSBAttr, uSBAttr :: AttrName
fSBAttr = "fSBAttr"
uSBAttr = "uSBAttr"

fCAttr, uCAttr :: AttrName
fCAttr = "fCAttr"
uCAttr = "uCAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (fCAttr, V.black `on` V.white)
  --, (uCAttr, V.white `on` V.black)
  --, (fSBAttr, V.white `on` V.blue)
  --, (uSBAttr, V.blue `on` V.red)
  ]
