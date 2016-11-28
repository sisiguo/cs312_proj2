-- 2048 Game in Haskell, Play user interaction driver
module Play where

-- To run it, try:
-- ghci
-- :load Play

import Game2048
import System.IO

-- play game2048 initGame

--  play :: Game -> IO State -> IO Result
play game startState  =
  -- let (wins, losses,ties) = tournament_state in
   do
      putStrLn ("Welcome to the 2048 game!")
      putStrLn ("This is your game board:")
      ss <- startState
      putStrLn (show ss)
      putStrLn ("To move, press 'U' for Up, 'D' for Down, 'L' for Left, and 'R' for Right.")
      line <- getLine
      -- Put below in a differnt function that will handle the remainder of the game loop
      -- the loop should handle Input move -> Output resulting board ... until get some sort of EOG (win/lose)?
      case (read line :: Char) of 'U' -> -- game (Move U ss) returns a IO Result ... new function to pattern match EOG or CG
                                  'D' -> -- and print the result and continue game loop?
                                  'L' -> ...
                                  'R' -> ...
      -- if (read line :: Int)==0
      -- then
      --       person_play game start opponent tournament_state
      -- else if (read line :: Int)==1
      --      then
      --       let ContinueGame state _ = start
      --          in person_play game (game (Move (opponent game start) state)) opponent tournament_state
      --       else
      --          return tournament_state

-- play game2048 initGame

-- person_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- -- opponent has played, the person must now play
-- person_play game (EndOfGame 1) opponent (wins,losses,ties) =
--    do
--       putStrLn "Computer won!"
--       play game (game Start) opponent (wins,losses+1,ties)
-- person_play game (EndOfGame 0) opponent (wins,losses,ties) =
--    do
--       putStrLn "I't a draw"
--       play game (game Start) opponent (wins,losses,ties+1)
-- person_play game (ContinueGame state avail) opponent tournament_state =
--    do
--       putStrLn ("State is "++show state++" choose one of "++show avail)
--       line <- getLine
--       computer_play game (game (Move (read line :: AMove) state)) opponent tournament_state


      