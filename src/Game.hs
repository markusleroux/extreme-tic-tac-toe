{-# LANGUAGE TemplateHaskell #-}

module Game where

import Data.Array
import Data.List (intercalate)
import Data.Foldable (asum)
import Data.Maybe (isJust, isNothing)

import Control.Applicative (Alternative, (<|>))

import Lens.Micro.TH (makeLenses)
import Lens.Micro.GHC (ix, _1, _2, (^.), (&), (%~), (.~), (?~), (^?!))

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
    displayAcross y y' = intercalate "  |  " [ displaySBRow x y y' | x <- [1..3] ]

    displaySBRow :: Int -> Int -> Int -> String
    displaySBRow x y y' = intercalate " | " [ displayCell $ ( b ! (x, y) ) ! (x', y') | x' <- [1..3] ]

data GameState =
  GS { _board :: Board          -- the board shown on the screen
     , _meta :: SubBoard        -- a record of who captured which squares
     , _player :: Move          -- the current player
     , _ppos :: Position        -- the subboard to play in
     , _cursor :: Position      -- the position of the highlighted square in the subboard (default (2,2))
     , _finished :: Maybe Move  -- flag describing who won the game, if there is a winner
     , _hasMoves :: Bool        -- flag describing whether the current subboard has any empty cells
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

play :: GameState -> GameState
play gs =
  if gs ^. hasMoves
    then playSquare gs
    else updateNoMoves $ gs & ppos .~ ( gs ^. cursor )          -- No available squares in SB

playSquare :: GameState -> GameState
playSquare gs =
  if isJust $ gs ^?! board . ix ( gs ^. ppos ) . ix ( gs ^. cursor )
    then gs
    else playEmptySquare gs

updateNoMoves :: GameState -> GameState
updateNoMoves gs = gs & hasMoves .~ ( notFull $ gs ^?! board . ix ( gs ^. ppos ) )
  where
    notFull :: SubBoard -> Bool
    notFull = any isNothing . elems

playEmptySquare :: GameState -> GameState
playEmptySquare = updateNoMoves . updateOthers . updateMeta . updateBoard       -- ORDER DEPENDENT
  where
    updateBoard :: GameState -> GameState
    updateBoard gs = gs & board . ix pos . ix ( gs ^. cursor ) ?~ ( gs ^. player )
      where
        pos :: Position
        pos = gs ^. ppos

    updateMeta :: GameState -> GameState
    updateMeta gs = gs & meta . ix pos %~ ( \ mvMb -> mvMb <|> ( hasWinnerMb $ gs ^?! board . ix pos ) )         -- will never be out of bounds
      where
        pos :: Position
        pos = gs ^. ppos

    updateOthers :: GameState -> GameState
    updateOthers gs =
      gs
        & ppos .~ ( gs ^. cursor )
        & finished .~ ( hasWinnerMb $ gs ^. meta )
        & player %~ ( \ p -> if p == X then O else X )

-- The list of three in a rows
winningRows :: [[Position]]
winningRows = [ [ ( i, j ) | i <- [1..3] ] | j <- [1..3] ] ++
       [ [ ( i, j ) | j <- [1..3] ] | i <- [1..3] ] ++
       [ [ ( i, i ) | i <- [1..3] ], [ ( i, 4 - i ) | i <- [1..3] ] ]

hasWinnerMb :: SubBoard -> Maybe Move
hasWinnerMb = asumMap winningRows $ allCaptured

-- Map a list of list of indices to a list of list of values. Then map the function with
-- signature [e] -> f a over this list, and apply <|> to the entries in the resulting list
asumMap :: ( Ix i, Alternative f ) => [[i]] -> ( [e] -> f a ) -> Array i e -> f a
asumMap rs p arr = asum [ p [ arr ! pos | pos <- r ] | r <- rs ]

-- Return Just Move if everything in the list is held by the same person otherwise return Nothing
allCaptured :: [Maybe Move] -> Maybe Move
allCaptured msMb = if and $ map ( == head msMb ) ( tail msMb ) then head msMb else Nothing

count :: Move -> SubBoard -> Int
count move = length . filter ( == Just move ) . elems
