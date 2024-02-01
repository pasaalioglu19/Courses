import Data.List (minimumBy,intercalate)
import Data.Ord (comparing)
import Control.Monad (when)

-- Enumerated type for piece
data Piece =
    Empty |
    Bishop |
    Knight |
    Rook |
    Queen |
    Pawn
    deriving (Show, Eq)

-- Enumerated type for team
data Team =
    EmptyC |
    Red |
    Green |
    Blue |
    Purple |
    White
    deriving (Show, Eq)

type Chessboard = [[(Piece, Team)]]

data Tree = Node Chessboard [Tree]

-- If you want to return all board value 
{- boardValue :: Chessboard -> Float
boardValue board = sum [pieceValue piece | row <- board, (piece, _) <- row] -}

boardValue :: Chessboard -> Float
boardValue board = minimumBy (comparing id) [boardValueHelper board t | t <- [Red, Green, Blue, Purple, White], boardValueHelper board t > 0.0]

boardValueHelper :: Chessboard -> Team -> Float
boardValueHelper board color = sum [pieceValue piece | row <- board, (piece, team) <- row, team == color]

horizontalCrossoverList :: Chessboard -> Int -> [Chessboard]
horizontalCrossoverList _ 0 = []
horizontalCrossoverList board x = (tail board ++ [head board]) : horizontalCrossoverList (tail board ++ [head board]) (x-1)

verticalCrossoverList :: Chessboard -> Int -> [Chessboard]
verticalCrossoverList _ 0 = []
verticalCrossoverList board x = [tail row ++ [head row] | row <- board] : verticalCrossoverList [tail row ++ [head row] | row <- board] (x-1)

deletePiece :: Chessboard -> (Int, Int) -> Chessboard
deletePiece board (x, y) = take x board ++ [take y row ++ [(Empty, EmptyC)] ++ drop (y + 1) row | row <- [board !! x]] ++ drop (x + 1) board

-- Helper function to assign values to pieces for board value calculation 
pieceValue :: Piece -> Float
pieceValue Queen = 10
pieceValue Rook = 5
pieceValue Bishop = 3.25
pieceValue Knight = 3.25
pieceValue Pawn = 1
pieceValue Empty = 0

attackCount :: Chessboard -> Int
attackCount board = sum [attackCountHelper board (x,y) | x <-[0..7], y <- [0..7]]

-- Helper function to determine if a piece results in an attack
attackCountHelper :: Chessboard -> (Int,Int) -> Int
attackCountHelper board (x,y)
    | fst (board !! x !! y) == Queen =
        sum (takeWhile (\i -> snd (board !! x !! (y - i)) /= team) [i1 | i1<-[1..y], snd (board !! x !! (y - i1)) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! x !! (y + i)) /= team) [i1 | i1<-[1..(7-y)], snd (board !! x !! (y + i1)) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! (x - i) !! y) /= team) [i1 | i1<-[1..x], snd (board !! (x - i1) !! y) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! (x + i) !! y) /= team) [i1 | i1<-[1..(7-x)], snd (board !! (x + i1) !! y) /= EmptyC ]) `min` 1 +

        sum (takeWhile (\i -> snd (board !! (x - i) !! (y - i)) /= team) [x1 | x1<-[1..min x y], snd (board !! (x - x1) !! (y - x1)) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! (x - i) !! (y + i)) /= team) [x1 | x1<-[1..min x (7-y)], snd (board !! (x - x1) !! (y + x1)) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! (x + i) !! (y - i)) /= team) [x1 | x1<-[1..min (7-x) y], snd (board !! (x + x1) !! (y - x1)) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! (x + i) !! (y + i)) /= team) [x1 | x1<-[1..min (7-x) (7-y)], snd (board !! (x + x1) !! (y + x1)) /= EmptyC ]) `min` 1

    | fst (board !! x !! y) == Rook =
        sum (takeWhile (\i -> snd (board !! x !! (y - i)) /= team) [i1 | i1<-[1..y], snd (board !! x !! (y - i1)) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! x !! (y + i)) /= team) [i1 | i1<-[1..(7-y)], snd (board !! x !! (y + i1)) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! (x - i) !! y) /= team) [i1 | i1<-[1..x], snd (board !! (x - i1) !! y) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! (x + i) !! y) /= team) [i1 | i1<-[1..(7-x)], snd (board !! (x + i1) !! y) /= EmptyC ]) `min` 1

    | fst (board !! x !! y) == Bishop =
        sum (takeWhile (\i -> snd (board !! (x - i) !! (y - i)) /= team) [i1 | i1<-[1..min x y], snd (board !! (x - i1) !! (y - i1)) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! (x - i) !! (y + i)) /= team) [i1 | i1<-[1..min x (7-y)], snd (board !! (x - i1) !! (y + i1)) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! (x + i) !! (y - i)) /= team) [i1 | i1<-[1..min (7-x) y], snd (board !! (x + i1) !! (y - i1)) /= EmptyC ]) `min` 1 +
        sum (takeWhile (\i -> snd (board !! (x + i) !! (y + i)) /= team) [i1 | i1<-[1..min (7-x) (7-y)], snd (board !! (x + i1) !! (y + i1)) /= EmptyC ]) `min` 1

    | fst (board !! x !! y) == Pawn =
        fromEnum (1 <= min x y) * fromEnum (snd (board !! (x - 1) !! (y - 1)) /= team && snd (board !! (x - 1) !! (y - 1)) /= EmptyC) +
        fromEnum (1 <= min x (7-y)) * fromEnum (snd (board !! (x - 1) !! (y + 1)) /= team && snd (board !! (x - 1) !! (y + 1)) /= EmptyC) +
        fromEnum (1 <= min (7-x) y) * fromEnum (snd (board !! (x + 1) !! (y - 1)) /= team && snd (board !! (x + 1) !! (y - 1)) /= EmptyC) +
        fromEnum (1 <= min (7-x) (7-y)) * fromEnum (snd (board !! (x + 1) !! (y + 1)) /= team && snd (board !! (x + 1) !! (y + 1)) /= EmptyC)

    | fst (board !! x !! y) == Knight =
        (if 1 <= min (x-1) y then fromEnum (snd (board !! (x - 2) !! (y - 1)) /= team && snd (board !! (x - 2) !! (y - 1)) /= EmptyC) else 0)
        + (if 1 <= min x (y-1) then fromEnum (snd (board !! (x - 1) !! (y - 2)) /= team && snd (board !! (x - 1) !! (y - 2)) /= EmptyC) else 0)

        + (if 1 <= min (x-1) (7-y) then fromEnum (snd (board !! (x - 2) !! (y + 1)) /= team && snd (board !! (x - 2) !! (y + 1)) /= EmptyC) else 0)
        + (if 1 <= min x (6-y) then fromEnum (snd (board !! (x - 1) !! (y + 2)) /= team && snd (board !! (x - 1) !! (y + 2)) /= EmptyC) else 0)

        + (if 1 <= min (6-x) y then fromEnum (snd (board !! (x + 2) !! (y - 1)) /= team && snd (board !! (x + 2) !! (y - 1)) /= EmptyC) else 0)
        + (if 1 <= min (7-x) (y-1) then fromEnum (snd (board !! (x + 1) !! (y - 2)) /= team && snd (board !! (x + 1) !! (y - 2)) /= EmptyC) else 0)

        + (if 1 <= min (6-x) (7-y) then fromEnum (snd (board !! (x + 2) !! (y + 1)) /= team && snd (board !! (x + 2) !! (y + 1)) /= EmptyC) else 0)
        + (if 1 <= min (7-x) (6-y) then fromEnum (snd (board !! (x + 1) !! (y + 2)) /= team && snd (board !! (x + 1) !! (y + 2)) /= EmptyC) else 0)

    | otherwise = 0
    where
        team = snd (board !! x !! y)

initialChessboard :: Chessboard
initialChessboard =
    [ [(Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Bishop, White), (Knight, Purple), (Empty, EmptyC),  (Empty, EmptyC)]
    , [(Empty, EmptyC), (Empty, EmptyC), (Queen, Purple), (Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC),  (Knight, Purple), (Empty, EmptyC)]
    , [(Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Knight, White), (Empty, EmptyC), (Rook, Green),    (Empty, EmptyC),  (Empty, EmptyC)]
    , [(Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC),  (Empty, EmptyC),  (Empty, EmptyC)]
    , [(Empty, EmptyC), (Empty, EmptyC), (Queen, Blue),   (Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC),  (Empty, EmptyC),  (Empty, EmptyC)]
    , [(Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC),  (Knight, Blue),   (Empty, EmptyC)]
    , [(Empty, EmptyC), (Empty, EmptyC), (Queen, Red),    (Empty, EmptyC), (Rook, Green),   (Bishop, White),  (Empty, EmptyC),  (Empty, EmptyC)]
    , [(Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Empty, EmptyC), (Bishop, White), (Empty, EmptyC),  (Empty, EmptyC),  (Empty, EmptyC)]
    ]

initialPieces :: Chessboard -> [(Int, Int)]
initialPieces board = [(x1,y1) | x1<-[0..7], y1<-[0..7], fst (board !! x1 !! y1 ) /= Empty]

allChessboards :: Chessboard -> [Chessboard]
allChessboards board = verticalCrossoverList board 7 ++ horizontalCrossoverList board 7 ++ [deletePiece board (x,y) | (x,y)<-initialPieces board]

bestChessboard1 :: [Chessboard] -> Chessboard
bestChessboard1 [] = error "Empty list"
bestChessboard1 boards = foldl1 (\board1 board2 -> if attackCount board1 > attackCount board2 then board2 else board1) boards

bestChessboard2 :: [Chessboard] -> Chessboard
bestChessboard2 [] = error "Empty list"
bestChessboard2 boards = foldl1 (\board1 board2 -> if boardValue board1 < boardValue board2 then board2 else board1) boards


newLevel :: Tree -> [Tree]
newLevel (Node board _) = map (`Node` []) (allChessboards board)

printBestBoards :: [Tree] -> Int -> Int -> IO ()
printBestBoards nodes level maxDepth
    | level <= maxDepth = do
        putStrLn $ "Level " ++ show level
        let allBoard = [a | Node a _ <- nodes]
        let bestBoard1 = bestChessboard1 allBoard
        let bestBoard2 = bestChessboard2 allBoard
        putStrLn $ "Best Attack Count: " ++ show (attackCount bestBoard1)
        printChessboard bestBoard1
        putStrLn $ "Best Min Value: " ++ show (boardValue bestBoard2)
        printChessboard bestBoard2

        let newLevelBoards = concatMap newLevel nodes
        when (level /= maxDepth) $ printBestBoards newLevelBoards (level + 1) maxDepth

main :: IO ()
main = do
    let initialTree = [Node initialChessboard []]
    printBestBoards initialTree 0 4 -- 4 is optional for level

-- Helper function to print the chessboard
printChessboard :: Chessboard -> IO ()
printChessboard = putStrLn . addBrackets . intercalate ",\n" . map showRow
  where
    addBrackets str = "[" ++ str ++ "]"
    showRow row = "[" ++ intercalate "," [showPiece piece team | (piece, team) <- row] ++ "]"
    showPiece Empty _ = "(Nil,Nil)"
    showPiece piece team = "(" ++ show piece ++ "," ++ show team ++ ")"