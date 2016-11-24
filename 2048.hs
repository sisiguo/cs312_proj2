-- 2048 Game in Haskell
module Game_2048 where

-- Imports
import System.Random

-- To run it, try:
-- ghci
-- :load 2048

-- Move is  either a 'U' for Up, 'D' for Down, 'L' for Left, or 'R' for right to move in a single direction
data Move = Up | Down | Left | Right

-- Tile is  an Int
type Tile = Int

-- Board is  a 2D grid of Tiles
type Board = [[Tile]]

-- An empty 4x4 game board
emptyBoard = [[0,0,0,0],
              [0,0,0,0],
              [0,0,0,0],
              [0,0,0,0]]

-- initBoard    initializes game board with 2 randomly chosen start tile locations and random start values of 2 or 4 for each
-- initBoard = TODO

-- getRandomValueNotEqualInRange r1 (x,y)    returns a random number within the range (x,y) not equal to r1
getRandomValueNotEqualInRange r1 (x,y) = do
    rand <- (randomRIO (x,y))
    if r1 == rand
        then getRandomValueNotEqualInRange r1 (x,y)
        else return rand
