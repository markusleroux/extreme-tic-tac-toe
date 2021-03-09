#!/usr/bin/env stack
-- stack --resolver lts-17.4 script

import qualified Text.Read as R
import Data.Array
import Data.List (intercalate)
import Data.Either.Combinators (maybeToLeft)
import Data.Foldable (asum)
import Control.Applicative (Alternative)
import Control.Monad.State

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
  GS { board :: Board
     , meta :: SubBoard
     }

updateGameState :: Move -> Position -> Position -> GameState -> GameState
updateGameState move pos newPos gs = GS nb nm
  where
    nb :: Board
    nb = updateBoard move pos newPos ( board gs )

    nm :: SubBoard
    nm = case hasWinnerSB $ board gs ! pos of
           Just m -> if meta gs ! pos == Nothing then meta gs // [(pos, Just m)] else meta gs
           Nothing -> meta gs

--------------------------

getPosition :: IO Position
getPosition =
  do x <- putStr "Column: " >> getInt
     y <- putStr "Row: " >> getInt
     return $ Position ( x, y )
  where
    getInt :: IO Int
    getInt =
      do xRaw <- getLine
         case R.readMaybe xRaw :: Maybe Int of
           Just x -> if x >= 1 && x <= 3 then return x else putStrLn "Please choose an integer in [1, 3]" >> getInt
           Nothing -> putStrLn "Invalid input." >> getInt

getEmptyPosition :: Position -> Board -> IO Position
getEmptyPosition pos b =
  do newPos <- getPosition
     if isEmpty newPos ( b ! pos ) then return newPos else putStrLn "Invalid move. Try again:" >> getEmptyPosition pos b
  where
    isEmpty :: Position -> SubBoard -> Bool
    isEmpty p sb = sb ! p == Nothing

-- The list of three in a rows
rows :: [[Position]]
rows = [ [Position (i, j) | i <- [1..3]] | j <- [1..3] ] ++
       [ [Position (i, j) | j <- [1..3]] | i <- [1..3] ] ++
       [ [Position (i, i) | i <- [1..3]], [Position (i, 4 - i) | i <- [1..3]] ]

hasWinnerSB :: SubBoard -> Maybe Move
hasWinnerSB = hasWinner rows $ allCaptured
  where
    -- Given a list of list of indices, apply the function with signature [e] -> f a to each
    -- list of values in the corresponding list of list of values. Then use <|> repeatedly on
    -- the resulting list [ f a ] to arrive at a single f a
    hasWinner :: ( Ix i, Alternative f ) => [[i]] -> ( [e] -> f a ) -> Array i e -> f a
    hasWinner rs p arr = asum ( map p [ [ arr ! pos | pos <- r ] | r <- rs ] )

    -- Return Just Move if everything in the list is held by the same person otherwise return Nothing
    allCaptured :: [Maybe Move] -> Maybe Move
    allCaptured mms = if and $ map ( == head mms ) ( tail mms ) then head mms else Nothing

-- Use hasWinnerBoard to check board at end of each round
checkWinner :: StateT GameState IO Position -> StateT GameState IO ( Either Move Position )
checkWinner = mapStateT $ liftM checkWinner'
  where
    -- Move from Maybe to Either
    checkWinner' :: (Position, GameState) -> (Either Move Position, GameState)
    checkWinner' (pos, gs) = (maybeToLeft pos ( hasWinnerSB $ meta gs ), gs)

playRound :: Move -> Position -> StateT GameState IO Position
playRound move pos =
  do gs <- get
     let b = board gs
     lift $ putStr $ displayBoard b
     lift $ putStrLn ( "Player " ++ show move ++ ", please choose a cell in square " ++ show pos ++ ".")
     newPos <- lift $ getEmptyPosition pos b
     put $ updateGameState move pos newPos gs
     return newPos

-- Similar to play round but with short circuit when a winner is found
playRoundAugmented :: Move -> Either Move Position -> StateT GameState IO ( Either Move Position )
playRoundAugmented move = either ( return . Left ) ( checkWinner . ( playRound move ) )

playGame :: Either Move Position -> StateT GameState IO ( Either Move Position )
playGame = foldl ( >=> ) return rounds
  where
    rounds :: [ Either Move Position -> StateT GameState IO ( Either Move Position ) ]
    rounds = map playRoundAugmented $ concat [[X, O] | _ <- [( 1 :: Int )..81]]

main :: IO ()
main =
  do pos <- putStrLn "Input the initial square to play in." >> getPosition
     (eitherMove, finalGS) <- runStateT ( playGame $ Right pos ) $ GS emptyBoard emptySubBoard
     case eitherMove of
       Left move -> putStrLn $ "Player " ++ show move ++ " wins!"
       Right _ -> putStrLn $ "Nobody wins."
     print $ displayBoard $ board finalGS
  where
    emptySubBoard :: SubBoard
    emptySubBoard = array (Position (1, 1), Position (3, 3)) [(Position (i, j), Nothing) | i <- [1..3], j <- [1..3]]

    emptyBoard :: Board
    emptyBoard = array (Position (1, 1), Position (3, 3)) [(Position (i, j), emptySubBoard) | i <- [1..3], j <- [1..3]]
