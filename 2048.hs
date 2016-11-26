-- 2048 Game in Haskell
module Game_2048 where

-- Imports
import System.Random

-- To run it, try:
-- ghci
-- :load 2048

-- AMove is  either a 'U' for Up, 'D' for Down, 'L' for Left, or 'R' for right to move in a single direction
data AMove = Up | Down | Left | Right

-- Action is    an action in the Game
data Action = Move AMove State  -- do AMove in State
            | Start State       -- start Game with State

-- Result is    the result of performing some Action on the Game
data Result = EndOfGame Int         -- end of game
            | ContinueGame State    -- continue with new State

type Game = Action -> Result

-- State is     is the state of the Game
type State = Board

-- Tile is  an Int
type Tile = Int

-- Board is  a 2D grid of Tiles
type Board = [[Tile]]

-- boardSize    is the size of a game board (number of tiles per row & column)
boardSize = 4

------ The 2048 Game -------

-- game2048 :: Game
-- game2048 (Move move state)
--     | move == 'U' = ...move tiles & check if any sum to 2048, if so EOG win, else cont.
--     | move == 'D' = ...if no more tiles to move (can merge), EOG lose
--     | move == 'L' = ...
--     | move == 'R' = ...
--     | otherwise = ...

-- game2048 (Start state) = ContinueGame state

-- initBoard    initializes game board with 2 randomly chosen start tile locations and random start values of 2 or 4 for each
-- initBoard :: IO Game
initBoard = do
    rI1 <- getRandomValueNotEqualInRange (-1) (0,boardSize)
    rI2 <- getRandomValueNotEqualInRange rI1 (0,boardSize)
    rT1 <- getRandomValueNotEqualInRange 3 (2,4)
    rT2 <- getRandomValueNotEqualInRange 3 (2,4)
    return (initGame rI1 rI2 rT1 rT2)

-- initGame rI1 rI2 rT1 rT2     nitializes the 2048 game with the given tile locations (rI1, rI2) and values (rT1, rT2)
-- initGame :: Int -> Int -> Int -> Int -> Game
initGame rI1 rI2 rT1 rT2 =  emptyBoard -- game2048 (Start ...builtBoard...)
    where
        -- emptyBoard is    an empty 4x4 game board
        emptyBoard = replicate boardSize [0 | v <- [1..boardSize]]
        i1 = rI1 `mod` boardSize
        i2 = rI2 `mod` boardSize
        -- board = [row | row <- emptyBoard !! i1]

-- getRandomValueNotEqualInRange r (x,y)    returns a random number within the range (x,y) not equal to r
getRandomValueNotEqualInRange :: Int -> (Int,Int) -> IO Int
getRandomValueNotEqualInRange r (x,y) = do
    rand <- (randomRIO (x,y))
    if r == rand
        then getRandomValueNotEqualInRange r (x,y)
        else return rand
