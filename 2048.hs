-- 2048 Game in Haskell
module Game_2048 where

-- Imports
-- import System.Random

-- To run it, try:
-- ghci
-- :load 2048

-- Move is  either a 'U' for Up, 'D' for Down, 'L' for Left, or 'R' for right to move in a single direction
data Move = Up
            | Down
            | Left
            | Right

-- Grid is  a 2D 4x4 grid of tiles
type Grid = [[Int]]

