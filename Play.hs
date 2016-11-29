-- Name 1: May Young
-- CSID: s5l8
-- Student #: 32950123

-- Name 2: Hui Si (Sisi) Guo
-- CSID: b7r8
-- Student #: 35674126


-- 2048 Game in Haskell, Play user interaction driver
module Play where

-- To run it, try:
-- ghci
-- :load Play

import Game2048
import System.IO
import Data.Char

-- Start the game with the below command:
-- play game2048 initGame

mediumNumMoves = hardNumMoves * 2
hardNumMoves = 520

-- play game state      starts the game
play :: Game -> IO State -> IO ()
play game state  = do
    putStrLn ("Welcome to the 2048 game!")
    putStr ("What level do you want to play at? Input 'E' for Easy, 'M' for Medium, or 'H' for Hard: ")
    level <- getLine
    case (toLower (level !! 0)) of
        'e' -> putStrLn("You get an unlimited number of moves to win the game.")
        'm' -> putStrLn("You get " ++ show mediumNumMoves ++ " moves to win the game.")
        'h' -> putStrLn("You get " ++ show hardNumMoves ++ " moves to win the game.")
    putStrLn ("This is your game board:")
    (startBoard,startNumMoves) <- state
    printBoard startBoard
    putStrLn ("To move, press 'U' for Up, 'D' for Down, 'L' for Left, and 'R' for Right.")
    putStr ("Input your first move: ")
    move <- getLine
    playLoop game move level (ContinueGame (startBoard,startNumMoves))

-- playLoop game move level state       allows the player to play until either they win or lose
playLoop :: Game -> [Char] -> [Char] -> Result -> IO ()
playLoop game move level (ContinueGame (board,numMoves)) = case (toLower (level !! 0)) of
    'e' -> loop
    'm' -> if (numMoves == mediumNumMoves)
            then putStrLn ("You've exceeded the maximum number of moves for the Medium difficulty level. You lose :(")
            else loop
    'h' -> if (numMoves == hardNumMoves)
            then putStrLn ("You've exceeded the maximum number of moves for the Hard difficulty level. You lose :(")
            else loop
    where
        loop = do
            case (toLower (move !! 0)) of
                'u' -> playLoop_helper U
                'd' -> playLoop_helper D
                'l' -> playLoop_helper L
                'r' -> playLoop_helper R
            where
                playLoop_helper m = do
                    next <- game (Move m (board,numMoves))
                    case next of
                        (ContinueGame (b,n)) -> do
                            printBoard b
                            putStrLn("The number of moves you've taken: " ++ show n)
                            putStr ("Input your next move: ")
                            line <- getLine
                            playLoop game line level next
                        (EndOfGame c) -> if (c == 1)
                            then putStrLn ("You win!")
                            else putStrLn ("No more moves. You lose :(")

-- printBoard board     prints a visual representation of the board to console 
printBoard :: Board -> IO ()
printBoard board = do
    putStrLn("---------------------") -- Top border
    let rows = [foldr (\x -> \b -> if x /= 0 
        then "|"++(foldr (\y -> \z -> " "++z) (show x) [1..(4-(length (show x)))])++b 
        else "|"++"    "++b) "|" row | row <- board]
    mapM_ (\x -> putStr (x ++ "\n" ++ "---------------------\n")) rows
