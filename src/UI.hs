{-# LANGUAGE OverloadedStrings #-}

module UI where

import Game

import Brick
import Brick.Widgets.Table

import Data.Array (array)
import Lens.Micro.GHC (ix, (^.), (^?!))

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
      , appAttrMap = theMap
      }

----------------------

handleEvent :: GameState -> BrickEvent Name () -> EventM Name ( Next GameState )
handleEvent gs (VtyEvent (V.EvKey V.KUp []))         = continue $ moveCursor North gs
handleEvent gs (VtyEvent (V.EvKey V.KDown []))       = continue $ moveCursor South gs
handleEvent gs (VtyEvent (V.EvKey V.KRight []))      = continue $ moveCursor East gs
handleEvent gs (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveCursor West gs
handleEvent gs (VtyEvent (V.EvKey V.KEnter []))      = continue $ play gs
handleEvent gs (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt gs
handleEvent gs (VtyEvent (V.EvKey V.KEsc []))        = halt gs
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'r') []))  = continue $ play initialState
handleEvent gs _                                     = continue gs

----------------------

drawUI :: GameState -> [ Widget Name ]
drawUI gs = return ( boardW gs <+> metaW gs )

metaW :: GameState -> Widget Name
metaW gs =
  C.vCenter
  $ padLeft ( Pad 10 )
  $ B.borderWithLabel ( str " Score " )
  $ subBoardW ( 1, 1 ) False ( gs ^. meta )

boardW :: GameState -> Widget Name
boardW gs =
  C.centerLayer
  $ B.borderWithLabel ( str " TicTacToe " )
  $ separateBorders
  $ renderTable
  $ surroundingBorder False
  $ setDefaultRowAlignment AlignMiddle
  $ setDefaultColAlignment AlignCenter
  $ table rowsOfSBW
  where
    rowsOfSBW :: [ [ Widget Name ] ]
    rowsOfSBW = [ [ getSBW ( x, y ) | x <- [1..3] ] | y <- [1..3] ]

    getSBW :: Position -> Widget Name
    getSBW posSB = let hasFocusedCell   = ( gs ^. hasMoves ) && posSB == ( gs ^. ppos )
                       isFocussedSB     = not ( gs ^. hasMoves ) && posSB == ( gs ^. cursor )
                       wSB              = subBoardW ( gs ^. cursor ) hasFocusedCell $ gs ^?! board . ( ix posSB ) in
      if isFocussedSB
         then withAttr fAttr wSB
         else wSB

subBoardW :: Position -> Bool -> SubBoard -> Widget Name
subBoardW posFC hasFocussedCell sb =
  padLeftRight 2
  $ padTopBottom 1
  $ separateBorders
  $ renderTable
  $ surroundingBorder False
  $ setDefaultRowAlignment AlignMiddle
  $ setDefaultColAlignment AlignCenter
  $ table cWs
  where
    cWs :: [ [ Widget Name ] ]
    cWs = [ [ getCW ( x, y ) | x <- [1..3] ] | y <- [1..3] ]

    getCW :: Position -> Widget Name
    getCW posC = let wC = cW $ sb ^?! ix posC in
      if hasFocussedCell && posC == posFC
         then withAttr fAttr wC
         else wC

    cW :: Cell -> Widget Name
    cW = padLeftRight 2 . str . displayCell

----------------------

fAttr :: AttrName
fAttr = "fAttr"

theMap :: GameState -> AttrMap
theMap gs = case gs ^. player of
              X -> attrMap V.defAttr [ (fAttr, V.black `on` V.blue) ]
              O -> attrMap V.defAttr [ (fAttr, V.black `on` V.yellow) ]

initialState :: GameState
initialState =
        GS { _board = emptyBoard 
           , _meta = emptySubBoard
           , _player = X
           , _ppos = ( 2, 2 )
           , _cursor = ( 2, 2 )
           , _finished = Nothing
           , _hasMoves = False                  -- choose initial sb
           }
        
emptySubBoard :: SubBoard
emptySubBoard = array ((1, 1), (3, 3)) [ ( ( x, y ), Nothing ) | x <- [1..3], y <- [1..3] ]

emptyBoard :: Board
emptyBoard = array ((1, 1), (3, 3)) [ ( ( x, y ), emptySubBoard ) | x <- [1..3], y <- [1..3] ]
