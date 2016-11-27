-- 2048 Game in Haskell
module Game_2048 where

-- Imports
import System.Random
import Data.List

-- To run it, try:
-- ghci
-- :load 2048

------ Data Structures -------

-- AMove is  either a 'U' for Up, 'D' for Down, 'L' for Left, or 'R' for right to move in a single direction
data AMove = U | D | L | R
        deriving(Eq)

-- Action is    an action in the Game
data Action = Move AMove State  -- do AMove in State
            | Start State       -- start Game with State

-- Result is    the result of performing some Action on the Game
data Result = EndOfGame Int         -- end of game (1 for win, -1 for lose)
            | ContinueGame State    -- continue with new State
        deriving (Show) -- TODO: keep? Added it so that initGame prints Result to console

------ Types -------

type Game = Action -> IO Result

-- State is     is the state of the Game
type State = Board

-- Tile is  an Int
type Tile = Int

-- Board is  a 2D grid of Tiles
type Board = [[Tile]]

------ Constants -------

-- boardSize    is the size of a game board (number of tiles per row & column)
boardSize = 4

------ The 2048 Game -------

game2048 :: Game
-- game2048     drives the 2048 game, takes an Action and outputs its Result
game2048 (Move aMove state) = do
    -- TODO:
        -- move tiles & check if (1 or more) sum to 2048, if so EOG win, else cont.
        -- if no more tiles can merge OR no more space to add new tiles, EOG lose
    if aMove == U then performMove state (move U state)
    else if aMove == D then performMove state (move D state)
    else if aMove == L then performMove state (move L state)
    else if aMove == R then performMove state (move R state)
    else return (EndOfGame (-1))
        -- where 
        -- 	mergedBoard ... = merge board -- return a tuple (isValid, boardState)
        -- 	newState ... = getNewState ...

game2048 (Start state) = do 
    return (ContinueGame state)

-- performMove currentState newState 	returns the result of performing a move
performMove :: State -> State -> IO Result
performMove currentState newState = do
    if currentState /= newState then (if (wonGame newState) then (return (EndOfGame 1)) else (getNextState newState))
    else return (ContinueGame currentState)

-- wonGame state 	returns True if there is a 2048 tile, False otherwise
wonGame :: State -> Bool
wonGame state = [] /= filter (== 2048) [e | v <- state, e <- v]

-- getNextState state 	returns the next state
getNextState :: State -> IO Result
getNextState state = do
    pos <- getRandomValueNotEqualInRange (-1) (0,boardSize)
    newVal <- getRandomValueNotEqualInRange 3 (2,4)
    let row = floor ((fromIntegral pos) / (fromIntegral boardSize)) 
        i = pos `mod` boardSize
        val = ((state !! row) !! i)
    if (val /= 0) then (getNextState state) else return (ContinueGame (addNewTile row i newVal state))

------ References -------
-- Inspired by Gregor Ulm's 2048 Implementation
-- See: http://gregorulm.com/2048-in-90-lines-haskell/

move :: AMove -> State -> State
move L = map merge
move R = map (reverse . merge . reverse)
move U = transpose . move L  . transpose
move D = transpose . move R . transpose

merge :: [Tile] -> [Tile]
merge xs = merged ++ padding
    where padding = replicate (length xs - length merged) 0
          merged  = combine $ filter (/= 0) xs
          combine (x:y:xs) | x == y = x * 2 : combine xs
                           | otherwise = x  : combine (y:xs)
          combine x = x 

------ Initialization -------

-- initGame    initializes the 2048 game with 2 randomly chosen start tile locations and random start values of 2 or 4 for each
initGame :: IO Result
initGame = do
    rI1 <- getRandomValueNotEqualInRange (-1) (0,boardSize)
    rI2 <- getRandomValueNotEqualInRange rI1 (0,boardSize)
    rT1 <- getRandomValueNotEqualInRange 3 (2,4)
    rT2 <- getRandomValueNotEqualInRange 3 (2,4)
    -- emptyBoard is    an empty (boardSize x boardSize) game board
    let emptyBoard = replicate boardSize [0 | v <- [1..boardSize]]
        row1 = floor ((fromIntegral rI1) / (fromIntegral boardSize))
        row2 = floor ((fromIntegral rI2) / (fromIntegral boardSize))
        i1 = rI1 `mod` boardSize
        i2 = rI2 `mod` boardSize
        tempBoard = addNewTile row1 i1 rT1 emptyBoard
        finalBoard = addNewTile row2 i2 rT2 tempBoard
    -- initializes the game board with the given tile locations (rI1, rI2) and values (rT1, rT2)
    game2048 (Start finalBoard)

-- getRandomValueNotEqualInRange r (x,y)    returns a random number within the range (x,y) not equal to r
getRandomValueNotEqualInRange :: Int -> (Int,Int) -> IO Int
getRandomValueNotEqualInRange r (x,y) = do
    rand <- (randomRIO (x,y))
    if r == rand
        then getRandomValueNotEqualInRange r (x,y)
        else return rand

-- addNewTile row i t board 	returns a new board with value t at index i of given row
addNewTile :: Int -> Int -> Int -> Board -> Board
addNewTile row i t board = part1 ++ [part2] ++ part3
    where
        part1 = take row board
        part2 = (take i (board !! row)) ++ [t] ++ (drop (i+1) (board !! row))
        part3 = drop (row+1) board

