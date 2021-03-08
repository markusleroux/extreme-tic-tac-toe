#!/usr/bin/env stack
-- stack --resolver lts-17.4 script

import qualified Text.Read as R
import Data.Array
import Data.List (intercalate)
import Data.Foldable
import Control.Applicative
import Control.Monad.State
--import Control.Monad.Trans.State.Lazy

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
displayBoard board = "\n" ++ intercalate longBar [ displaySBRow y | y <- [1..3] ] ++ "\n\n"
  where
    longBar = "\n-------------------------------------\n"
    longGappedBar = "\n---------- | ----------- | ----------\n"

    displaySBRow :: Int -> String
    displaySBRow y = intercalate longGappedBar [ intercalate "  |  " ( displayAcross y y' ) | y' <- [1..3] ]

    displayAcross :: Int -> Int -> [String]
    displayAcross y y' = [intercalate " | " [displayCell ( ( board ! Position (x, y) ) ! Position (x', y') ) | x' <- [1..3]] | x <- [1..3]]

--------------------------

updateBoard :: Move -> Position -> Position -> Board -> Board
updateBoard move pos newPos board = board // [(pos, newSubBoard)]
  where newSubBoard = board ! pos // [(newPos, Just move)]

getPosition :: IO Position
getPosition =
  let getInt =
        do xRaw <- getLine
           case R.readMaybe xRaw :: Maybe Int of
             Just x -> return x
             Nothing -> putStrLn "Invalid move." >> getInt
  in
    do putStr "Column: "
       x <- getInt
       putStr "Row: "
       y <- getInt
       return $ Position ( x, y )

getEmptyPosition :: Position -> Board -> IO Position
getEmptyPosition pos board =
  do newPos <- getPosition
     if isEmpty newPos ( board ! pos ) then return newPos else getEmptyPosition pos board
  where
    isEmpty :: Position -> SubBoard -> Bool
    isEmpty pos sb = sb ! pos == Nothing

-- The list of three in a rows
rows :: [[Position]]
rows = [ [Position (i, j) | i <- [1..3]] | j <- [1..3] ] ++
       [ [Position (i, j) | j <- [1..3]] | i <- [1..3] ] ++
       [ [Position (i, i) | i <- [1..3]], [Position (i, 4 - i) | i <- [1..3]] ]

-- Return Just Move if everything in the list is held by the same person otherwise return Nothing
allCaptured :: [Maybe Move] -> Maybe Move
allCaptured mms = if and $ map ( == head mms ) ( tail mms ) then head mms else Nothing

-- Given a list of list of indices, apply the function with signature [e] -> f a to each
-- list of values in the corresponding list of list of values. Then use <|> repeatedly on
-- the resulting list [ f a ] to arrive at a single f a
hasWinner :: ( Ix i, Alternative f ) => [[i]] -> ( [e] -> f a ) -> Array i e -> f a
hasWinner rs p arr = asum ( map p [ [ arr ! pos | pos <- r ] | r <- rs ] )

hasWinnerBoard :: Board -> Maybe Move
hasWinnerBoard = hasWinner rows $ allCaptured . ( map $ hasWinner rows allCaptured )

-- Use hasWinnerBoard to check board at end of each round
checkWinner :: StateT Board IO Position -> StateT Board IO ( Either Move Position )
checkWinner = mapStateT $ liftM checkWinner'
  where
    -- Move from Maybe to Either
    checkWinner' :: (Position, Board) -> (Either Move Position, Board)
    checkWinner' (pos, board) = case hasWinnerBoard board of
                                  Just move -> (Left move, board)
                                  Nothing -> (Right pos, board)

playRound :: Move -> Position -> StateT Board IO Position
playRound move pos =
  do board <- get
     lift $ putStr $ displayBoard board
     lift $ putStrLn ( "Player " ++ show move ++ ", please choose a cell in square " ++ show pos ++ ".")
     newPos <- lift $ getEmptyPosition pos board
     put $ updateBoard move pos newPos board
     return newPos

-- Similar to play round but with short circuit when a winner is found
playRoundAugmented :: Move -> Either Move Position -> StateT Board IO ( Either Move Position )
playRoundAugmented move = either ( return . Left ) ( checkWinner . ( playRound move ) )

playGame :: Either Move Position -> StateT Board IO ( Either Move Position )
playGame = foldl ( >=> ) return rounds
  where
    rounds :: [ Either Move Position -> StateT Board IO ( Either Move Position ) ]
    rounds = map playRoundAugmented $ concat [[X, O] | _ <- [( 1 :: Int )..81]]

main :: IO ()
main =
  do putStrLn "Input the initial square to play in."
     pos <- getPosition
     finalBoard <- execStateT ( playGame $ Right pos ) emptyBoard
     print $ displayBoard finalBoard
  where
    emptySubBoard :: SubBoard
    emptySubBoard = array (Position (1, 1), Position (3, 3)) [(Position (i, j), Nothing) | i <- [1..3], j <- [1..3]]

    emptyBoard :: Board
    emptyBoard = array (Position (1, 1), Position (3, 3)) [(Position (i, j), emptySubBoard) | i <- [1..3], j <- [1..3]]
