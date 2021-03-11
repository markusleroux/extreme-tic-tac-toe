#!/usr/bin/env stack
-- stack --resolver lts-17.4 script

{-# LANGUAGE TemplateHaskell #-}

import Data.Array
import Data.List (intercalate)
import Data.Foldable (asum)
import Data.Maybe (isJust)
import Control.Applicative (Alternative)

import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (&), (%~), (.~))

--------------------------

data Move = X | O deriving (Show, Eq)
newtype Position = Position ( Int, Int ) deriving (Eq, Ord, Ix)

instance Show Position where
  show ( Position (x, y) ) = show ( x, y )

type Cell = Maybe Move

displayCell :: Cell -> String
displayCell ( Just move ) = show move
displayCell Nothing = " "

type SubBoard = Array Position Cell
type Board = Array Position SubBoard

displayBoard :: Board -> String
displayBoard b = "\n" ++ intercalate longBar [ displaySBRow y | y <- [1..3] ] ++ "\n\n"
  where
    longBar = "\n-------------------------------------\n"
    longGappedBar = "\n---------- | ----------- | ----------\n"

    -- display subboards in one row
    displaySBRow :: Int -> String
    displaySBRow y = intercalate longGappedBar [ displayAcross y y' | y' <- [1..3] ]

    -- display entire row across multiple subboards
    displayAcross :: Int -> Int -> String
    displayAcross y y' = intercalate "  |  " [intercalate " | " [displayCell $ getCell x y x' y' | x' <- [1..3]] | x <- [1..3]]

    getCell :: Int -> Int -> Int -> Int -> Cell
    getCell x y x' y' = ( b ! Position (x, y) ) ! Position (x', y')

updateBoard :: Move -> Position -> Position -> Board -> Board
updateBoard move pos newPos b = b // [(pos, newSubBoard)]
  where newSubBoard = b ! pos // [(newPos, Just move)]

data GameState =
  GS { _board :: Board
     , _meta :: SubBoard
     , _player :: Move
     , _ppos :: Position
     , _finished :: Maybe Move
     } deriving (Show)

$(makeLenses ''GameState)

playRound :: Position -> GameState -> GameState
playRound newPos gs =
  gs
    & player %~ ( \ p -> if p == X then O else X )
    & board %~ updateBoard ( gs ^. player ) pos newPos
    & meta .~ updatedMeta
    & ppos .~ newPos
    & finished .~ ( hasWinnerSB $ gs ^. meta )
  where
    pos :: Position
    pos = gs ^. ppos

    updatedMeta :: SubBoard
    updatedMeta =
      if ( gs ^. meta ) ! pos == Nothing && isJust ( hasWinnerSB $ ( gs ^. board ) ! ( gs ^. ppos ) )
        then ( gs ^. meta ) // [(pos, Just $ gs ^. player)]
        else gs ^. meta

--------------------------

-- The list of three in a rows
rows :: [[Position]]
rows = [ [Position (i, j) | i <- [1..3]] | j <- [1..3] ] ++
       [ [Position (i, j) | j <- [1..3]] | i <- [1..3] ] ++
       [ [Position (i, i) | i <- [1..3]], [Position (i, 4 - i) | i <- [1..3]] ]

hasWinnerSB :: SubBoard -> Maybe Move
hasWinnerSB = asumMap rows $ allCaptured
  where
    -- Given a list of list of indices, apply the function with signature [e] -> f a to each
    -- list of values in the corresponding list of list of values. Then use <|> repeatedly on
    -- the resulting list [ f a ] to arrive at a single f a
    asumMap :: ( Ix i, Alternative f ) => [[i]] -> ( [e] -> f a ) -> Array i e -> f a
    asumMap rs p arr = asum ( map p [ [ arr ! pos | pos <- r ] | r <- rs ] )

    -- Return Just Move if everything in the list is held by the same person otherwise return Nothing
    allCaptured :: [Maybe Move] -> Maybe Move
    allCaptured msMb = if and $ map ( == head msMb ) ( tail msMb ) then head msMb else Nothing
