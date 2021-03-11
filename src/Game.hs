{-# LANGUAGE TemplateHaskell #-}

module Game where

import Data.Array
import Data.List (intercalate)
import Data.Foldable (asum)
import Data.Maybe (isJust)

import Control.Applicative (Alternative)
import Control.Lens.Tuple

import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (&), (%~), (.~))

-- Types
--------------------------

data Move = X | O deriving (Show, Eq)
type Position = ( Int, Int )
type Cell = Maybe Move

displayCell :: Cell -> String
displayCell ( Just move ) = show move
displayCell Nothing = " "

type SubBoard = Array Position Cell
type Board = Array Position SubBoard

displayBoard :: Board -> String
displayBoard b = "\n" ++ intercalate longBar [ displayBoardRow y | y <- [1..3] ] ++ "\n\n"
  where
    longBar = "\n-------------------------------------\n"
    longGappedBar = "\n---------- | ----------- | ----------\n"

    -- display subboards in one row
    displayBoardRow :: Int -> String
    displayBoardRow y = intercalate longGappedBar [ displayAcross y y' | y' <- [1..3] ]

    -- display entire row across multiple subboards
    displayAcross :: Int -> Int -> String
    displayAcross y y' = intercalate "  |  " [ displaySBRow x y y' | x <- [1..3]]

    displaySBRow :: Int -> Int -> Int -> String
    displaySBRow x y y' = intercalate " | " [displayCell $ ( b ! (x, y) ) ! (x', y') | x' <- [1..3]]

updateBoard :: Move -> Position -> Position -> Board -> Board
updateBoard move pos newPos b = b // [(pos, newSubBoard)]
  where newSubBoard = b ! pos // [(newPos, Just move)]

data GameState =
  GS { _board :: Board          -- the board shown on the screen
     , _meta :: SubBoard        -- a record of who captured which squares
     , _player :: Move          -- the current player
     , _ppos :: Position        -- the subboard to play in
     , _cursor :: Position      -- the position of the highlighted square in the subboard (default (2,2))
     , _finished :: Maybe Move  -- flag describing who won the game, if there is a winner
     } deriving (Show)

$(makeLenses ''GameState)

data Direction
  = North
  | South
  | West
  | East
  deriving (Eq, Show)

moveCursor :: Direction -> GameState -> GameState
moveCursor North = cursor . _2 %~ ( \ y -> if y > 1 then y - 1 else y )
moveCursor South = cursor . _2 %~ ( \ y -> if y < 3 then y + 1 else y )
moveCursor East = cursor . _1 %~ ( \ x -> if x < 3 then x + 1 else x )
moveCursor West = cursor . _1 %~ ( \ x -> if x > 1 then x - 1 else x )

-- Game logic
--------------------------

playSquare :: GameState -> GameState
playSquare gs =
  if isJust $ ( ( gs ^. board ) ! ( gs ^. ppos ) ) ! ( gs ^. cursor )
     then gs
     else playEmptySquare gs

playEmptySquare :: GameState -> GameState
playEmptySquare gs =
  gs
    & player %~ ( \ p -> if p == X then O else X )
    & board %~ updateBoard ( gs ^. player ) pos ( gs ^. cursor )
    & meta .~ updatedMeta
    & ppos .~ ( gs ^. cursor )
    & finished .~ ( hasWinnerMb $ gs ^. meta )
  where
    pos :: Position
    pos = gs ^. ppos            -- is this a risk?

    updatedMeta :: SubBoard
    updatedMeta =
      if ( gs ^. meta ) ! pos == Nothing && isJust ( hasWinnerMb $ ( gs ^. board ) ! ( gs ^. ppos ) )
        then ( gs ^. meta ) // [ (pos, Just $ gs ^. player) ]
        else gs ^. meta
             
-- The list of three in a rows
rows :: [[Position]]
rows = [ [ (i, j) | i <- [1..3]] | j <- [1..3] ] ++
       [ [ (i, j) | j <- [1..3]] | i <- [1..3] ] ++
       [ [ (i, i) | i <- [1..3]], [(i, 4 - i) | i <- [1..3]] ]

hasWinnerMb :: SubBoard -> Maybe Move
hasWinnerMb = asumMap rows $ allCaptured
  where
    -- Given a list of list of indices, apply the function with signature [e] -> f a to each
    -- list of values in the corresponding list of list of values. Then use <|> repeatedly on
    -- the resulting list [ f a ] to arrive at a single f a
    asumMap :: ( Ix i, Alternative f ) => [[i]] -> ( [e] -> f a ) -> Array i e -> f a
    asumMap rs p arr = asum ( map p [ [ arr ! pos | pos <- r ] | r <- rs ] )

    -- Return Just Move if everything in the list is held by the same person otherwise return Nothing
    allCaptured :: [Maybe Move] -> Maybe Move
    allCaptured msMb = if and $ map ( == head msMb ) ( tail msMb ) then head msMb else Nothing
