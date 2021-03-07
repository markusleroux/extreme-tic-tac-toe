import Data.Array
import Data.Foldable
import Control.Applicative
import Control.Monad.State
--import Control.Monad.Trans.State.Lazy

data Move = X | O deriving (Show, Eq)
newtype Position = Position ( Int, Int ) deriving (Show, Eq, Ord, Ix)
type Cell = Maybe Move

type SubBoard = Array Position Cell
type Board = Array Position SubBoard

updateBoard :: Move -> Position -> Position -> Board -> Board
updateBoard move pos newPos board = board // [(pos, newSubBoard)]
  where newSubBoard = board ! pos // [(newPos, Just move)]

getPosition :: IO Position
getPosition =
  do x <- getLine
     y <- getLine
     return $ Position ( read x :: Int, read y :: Int )

isEmpty :: Position -> SubBoard -> Bool
isEmpty pos sb = sb ! pos == Nothing

playRound :: Move -> Position -> StateT Board IO Position
playRound move pos =
  do board <- get
     newPos <- lift getPosition
     put $ updateBoard move pos newPos board
     return newPos

-- The list of three in a rows
rows :: [[Position]]
rows = [ [Position (i, j) | i <- [1..3]] | j <- [1..3] ] ++
       [ [Position (i, j) | j <- [1..3]] | i <- [1..3] ] ++
       [ [Position (i, i) | i <- [1..3]], [Position (i, 4 - i) | i <- [1..3]] ]

-- Return Just Move if everything in the list is held by the same person otherwise return Nothing
allCaptured :: [Maybe Move] -> Maybe Move
allCaptured mm = if and $ map ( == head mm ) ( tail mm ) then head mm else Nothing

-- Given a list of list of indices, apply the function with signature [e] -> f a to each
-- list of values in the corresponding list of list of values. Then use <|> repeatedly on
-- the resulting list [ f a ] to arrive at a single f a
hasWinner :: ( Ix i, Alternative f ) => [[i]] -> ( [e] -> f a ) -> Array i e -> f a
hasWinner rows pred array = asum ( map pred [ [ array ! pos | pos <- row ] | row <- rows ] )

hasWinnerBoard :: Board -> Maybe Move
hasWinnerBoard = hasWinner rows $ allCaptured . ( map $ hasWinnerSubBoard )
  where
    hasWinnerSubBoard :: SubBoard -> Maybe Move
    hasWinnerSubBoard = hasWinner rows allCaptured

-- Use hasWinnerBoard to check board at end of each round
checkWinner :: StateT Board IO Position -> StateT Board IO ( Either Move Position )
checkWinner = mapStateT $ liftM checkWinner'
  where
    -- Move from Maybe to Either
    checkWinner' :: (Position, Board) -> (Either Move Position, Board)
    checkWinner' (pos, board) = case hasWinnerBoard board of
                                  Just move -> (Left move, board)
                                  Nothing -> (Right pos, board)

-- Similar to play round but with short circuit when a winner is found
playRoundAugmented :: Move -> Either Move Position -> StateT Board IO ( Either Move Position )
playRoundAugmented move e = case e of
                              Right pos -> checkWinner $ playRound move pos
                              Left a -> return ( Left a )


playGame :: Either Move Position -> StateT Board IO ( Either Move Position )
playGame = foldl ( >=> ) return rounds
  where
    rounds :: [ Either Move Position -> StateT Board IO ( Either Move Position ) ]
    rounds = map playRoundAugmented $ concat [[X, O] | _ <- [( 1 :: Int )..81]]

main :: IO ()
main =
  do pos <- getPosition
     finalBoard <- execStateT ( playGame $ Right pos ) emptyBoard
     print $ show finalBoard
  where
    emptySubBoard :: SubBoard
    emptySubBoard = array (Position (1, 1), Position (3, 3)) [(Position (i, j), Nothing) | i <- [1..3], j <- [1..3]]

    emptyBoard :: Board
    emptyBoard = array (Position (1, 1), Position (3, 3)) [(Position (i, j), emptySubBoard) | i <- [1..3], j <- [1..3]]
