-- 2048 Game in Haskell, Play user interaction driver
module Play where

-- To run it, try:
-- ghci
-- :load Play

import Game2048
import System.IO

-- play game2048 initGame

play :: Game -> IO State -> IO ()
play game state  = do
    putStrLn ("Welcome to the 2048 game!")
    putStrLn ("This is your game board:")
    startState <- state
    putStrLn (show startState)
    putStrLn ("To move, press 'U' for Up, 'D' for Down, 'L' for Left, and 'R' for Right.")
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
                    putStrLn (show next)
                    line <- getLine
                    playLoop game line next
                (EndOfGame c) -> if (c == 1)
                    then putStrLn ("You win!")
                    else putStrLn ("No more moves. You lost :(") -- TODO: give option to restart?

-- TODO: function to format board output
printBoard :: Board -> IO String
printBoard board = do
    putStrLn("---------------------") -- Top border
    putStrLn("---------------------") -- Bottom border


      