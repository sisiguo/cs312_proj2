-- 2048 Game in Haskell, Play user interaction driver
module Play where

-- To run it, try:
-- ghci
-- :load Play

import Game2048
import System.IO
import Data.Char

-- play game2048 initGame

play :: Game -> IO State -> IO ()
play game state  = do
    putStrLn ("Welcome to the 2048 game!")
    putStrLn ("This is your game board:")
    startState <- state
    printBoard startState
    putStrLn ("To move, press 'U' for Up, 'D' for Down, 'L' for Left, and 'R' for Right.")
    putStrLn ("Input your first move:")
    line <- getLine
    playLoop game line (ContinueGame startState)

playLoop :: Game -> [Char] -> Result -> IO ()
playLoop game move (ContinueGame state) = do
    case (toLower (move !! 0)) of
        'u' -> playLoop_helper U
        'd' -> playLoop_helper D
        'l' -> playLoop_helper L
        'r' -> playLoop_helper R
    where
        playLoop_helper m = do
            next <- game (Move m state)
            case next of
                (ContinueGame s) -> do
                    printBoard s
                    putStrLn ("Input your next move:")
                    line <- getLine
                    playLoop game line next
                (EndOfGame c) -> if (c == 1)
                    then putStrLn ("You win!")
                    else putStrLn ("No more moves. You lost :(") -- TODO: give option to restart?

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn("---------------------") -- Top border
    let test1 = [foldr (\x -> \b -> if x /= 0 
        then "|"++(foldr (\y -> \z -> " "++z) (show x) [1..(4-(length (show x)))])++b 
        else "|"++"    "++b) "|" row | row <- board]
    mapM_ putStrLn test1
    putStrLn("---------------------") -- Bottom borde





      