module Main where

import Data.List (nub, permutations)
import Data.Maybe (catMaybes)
import Control.Monad (guard)

-- A position on the 9x9 grid
type Pos = (Int, Int)

-- A cage is a list of positions and a target sum
data Cage = Cage { positions :: [Pos], targetSum :: Int } deriving (Show)

-- The grid is a list of (position, value) pairs; empty cells are not in the list
type Grid = [(Pos, Int)]

-- Standard Sudoku rules: rows, columns, and 3x3 boxes
rows :: [[Pos]]
rows = [[(x, y) | x <- [0..8]] | y <- [0..8]]

cols :: [[Pos]]
cols = [[(x, y) | y <- [0..8]] | x <- [0..8]]

boxes :: [[Pos]]
boxes = [[(x + dx, y + dy) | dx <- [0..2], dy <- [0..2]] 
         | x <- [0, 3, 6], y <- [0, 3, 6]]

-- Check if a grid satisfies Sudoku rules
isValidSudoku :: Grid -> Bool
isValidSudoku grid = all (distinct . vals) rows && 
                     all (distinct . vals) cols && 
                     all (distinct . vals) boxes
  where
    vals ps = [v | (p, v) <- grid, p `elem` ps]
    distinct xs = length xs == length (nub xs)

-- Check if a grid satisfies cage constraints
isValidCages :: [Cage] -> Grid -> Bool
isValidCages cages grid = all checkCage cages
  where
    checkCage (Cage ps sum_) = 
      let vs = [v | (p, v) <- grid, p `elem` ps]
      in length vs == length ps && sum vs == sum_ && distinct vs
    distinct xs = length xs == length (nub xs)

-- Find empty positions not yet assigned in the grid
emptyPositions :: Grid -> [Pos]
emptyPositions grid = [(x, y) | x <- [0..8], y <- [0..8], 
                       not $ any (\(p, _) -> p == (x, y)) grid]

-- Simple backtracking solver
solve :: [Cage] -> Grid -> [Grid]
solve cages grid 
  | not (isValidSudoku grid) || not (isValidCages cages grid) = []
  | null empties = [grid] -- Solved if no empty positions
  | otherwise = do
      let pos = head empties
      val <- [1..9]
      let newGrid = (pos, val) : grid
      solve cages newGrid
  where
    empties = emptyPositions grid

-- Example puzzle: a small 4x4 grid for simplicity (adapt to 9x9 as needed)
exampleCages :: [Cage]
exampleCages = [Cage [(0,0), (0,1)] 7, Cage [(1,0), (1,1)] 8]
exampleGrid :: Grid
exampleGrid = []

-- Test the solver
main :: IO ()
main = do
  let solutions = take 1 $ solve exampleCages exampleGrid
  mapM_ print solutions
