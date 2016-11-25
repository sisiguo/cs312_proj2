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

------ The 2048 Game -------

game2048 :: Game
game2048 (Move move state)
    | move == 'U' = ...move tiles & check if any sum to 2048, if so EOG win, else cont.
    | move == 'D' = ...if no more tiles to move (can merge), EOG lose
    | move == 'L' = ...
    | move == 'R' = ...
    | otherwise = ...

game2048 (Start state) = ContinueGame state

-- emptyBoard is    an empty 4x4 game board
emptyBoard = [[0,0,0,0],
              [0,0,0,0],
              [0,0,0,0],
              [0,0,0,0]]

-- initBoard    initializes game board with 2 randomly chosen start tile locations and random start values of 2 or 4 for each
initBoard = do
    rT1 <- ...
    rT2 <- ...
    rV1 <- ...
    rV2 <- ...
    initGame rT1 rT2 rV1 rV2

initGame rT1 rT2 rV1 rV2 = game2048 (Start ...builtBoard...)
    where
        boardSize = 4
        emptyBoard = emptyBoard

-- getRandomValueNotEqualInRange r1 (x,y)    returns a random number within the range (x,y) not equal to r1
getRandomValueNotEqualInRange r1 (x,y) = do
    rand <- (randomRIO (x,y))
    if r1 == rand
        then getRandomValueNotEqualInRange r1 (x,y)
        else return rand
