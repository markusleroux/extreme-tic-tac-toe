{-# LANGUAGE OverloadedStrings #-}

module UI where

import Game

import Brick
import Brick.Widgets.Table

import Data.Array ((!))
import Lens.Micro ((^.))

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
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
drawUI gs = return ( boardW gs <+> scoresW gs )

scoresW :: GameState -> Widget Name
scoresW gs = let met = gs ^. meta in
  C.vCenter
  $ padLeft ( Pad 10 )
  $ hLimit 8
  $ vLimit 4
  $ vBox [ C.hCenter $ str "Score"
         , C.hCenter $ B.hBorder
         , C.hCenter $ scoreW X met
         , C.hCenter $ scoreW O met
         ]

scoreW :: Move -> SubBoard -> Widget Name
scoreW mv sb = str $ show mv ++ ": " ++ ( show $ count mv sb )

boardW :: GameState -> Widget Name
boardW gs = C.centerLayer
  $ B.borderWithLabel ( str " TicTacToe " )
  $ separateBorders
  $ renderTable
  $ surroundingBorder False
  $ setDefaultRowAlignment AlignMiddle
  $ setDefaultColAlignment AlignCenter
  $ table rowsOfSBW
  where
    rowsOfSBW :: [ [ Widget Name ] ]
    rowsOfSBW = [ [ getSBW x y | x <- [1..3] ] | y <- [1..3] ]

    getSBW :: Int -> Int -> Widget Name
    getSBW x y = let ( fx, fy ) = gs ^. cursor
                     isFocus = ( x, y ) == ( gs ^. ppos )
                     w = subBoardW fx fy isFocus $ ( gs ^. board ) ! ( x, y ) in
      if isFocus
         then withAttr fSBAttr w
         else w


subBoardW :: Int -> Int -> Bool -> SubBoard -> Widget Name
subBoardW fx fy isFocus sb =
  padLeftRight 2
  $ padTopBottom 1
  $ separateBorders
  $ renderTable
  $ surroundingBorder False
  $ setDefaultRowAlignment AlignMiddle
  $ setDefaultColAlignment AlignCenter
  $ table rows
  where
    rows :: [ [ Widget Name ] ]
    rows = [ [ getCW x y | x <- [1..3] ] | y <- [1..3] ]

    getCW :: Int -> Int -> Widget Name
    getCW x y = let w = cW $ sb ! ( x, y ) in
      if isFocus && ( x, y ) == ( fx, fy )
         then withAttr fCAttr w
         else w

    cW :: Cell -> Widget Name
    cW = padLeftRight 2 . str . displayCell

----------------------

fSBAttr :: AttrName
fSBAttr = "fSBAttr"

fCAttr :: AttrName
fCAttr = "fCAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (fCAttr, V.black `on` V.white)
  --, (fSBAttr, V.white `on` V.blue)
  ]
