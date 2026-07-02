{-# LANGUAGE LambdaCase #-}
import System.IO
import Data.Char
import Data.List
import Data.Maybe

{- A sudoku solver in Haskell that reads the puzzles from a txt file and returns the solutions -}

data Cell = Filled Int | Empty [Int] deriving (Eq, Show)
type Grid = [[Cell]]
type Loc = (Int, Int)

splitFileToGrids :: [String] -> [[String]]
splitFileToGrids [] = []
splitFileToGrids file = let (puzzle, rest) = splitAt 10 file in puzzle : splitFileToGrids rest

{- returns the list of cells from a given string representation of a row -}
convertToRow :: String -> [Cell]
convertToRow = map f
    where f '0' = Empty [1..9]
          f cell = Filled (digitToInt cell)

convertToGrid :: [String] -> Grid
convertToGrid = map convertToRow

{- returns a list of fixed values in a line of cells -}
getFixedValues :: [Cell] -> [Int]
getFixedValues = map (\case Filled x -> x; Empty _ -> 0)

{- returns the values that are not allowed to be used in that cell -}
getAllFixedValues :: Loc -> Grid -> [Int]
getAllFixedValues (i, j) grid =
    [ value | (i', row) <- zip [0..] grid, (j', Filled value) <- zip [0..] row,
              i == i' || j == j' || i `div` 3 == i' `div` 3 && j `div` 3 == j' `div` 3]

{- filters the domain of a cell from the given location -}
filterLocDomain :: Loc -> Grid -> [Int]
filterLocDomain (i, j) grid =
    let
        fixedValues = getAllFixedValues (i, j) grid
        in [1..9] \\ fixedValues

{- returns the filtered row based on the given index -}
filterRow :: Grid -> Int -> [Cell]
filterRow grid i = do
    j <- [0..8]
    let cell = grid !! i !! j
        newDomain = filterLocDomain (i, j) grid
        in
            case cell of
                Filled _ -> [cell]
                Empty _ -> [Empty newDomain]

{- filters the domain of each cell on the grid -}
filterGrid :: Grid -> Grid
filterGrid grid = map (filterRow grid) [0 .. 8]

{- return the domain of a given variable -}
getDomain :: Loc -> Grid -> [Int]
getDomain (row, col) grid =
    let cell = grid !! row !! col
        in
            case cell of
                Filled _ -> []
                Empty domain -> domain

{- removes a given value from a given domain -}
updateDomain :: [Int] -> Int -> [Int]
updateDomain domain value = domain \\ [value]

{- minimum remaining values heuristic, returns the location of the cell with the least number of possible values -}
mrv :: Grid -> Maybe (Int, [Int], Loc)
mrv grid =
    case [(length vals, vals, (i, j)) | (i, row) <- zip [0..] grid,
                                  (j, Empty vals) <- zip [0..] row] of
        [] -> Nothing
        l -> Just (minimum l)

clearDom :: Cell -> Int -> Cell
clearDom cell n =
    case cell of
        Filled _ -> cell
        Empty domain -> Empty (updateDomain domain n)

{- returns the given row after inserting a value at a given location -}
updateRelative :: Grid -> (Loc, Int) -> [Cell]
updateRelative grid ((i,j), val) = 
    [ if (i,j) == (i',j') then Filled val else val' | (i', row) <- zip [0..] grid, (j', temp) <- zip [0..] row,
        let 
            val' =
                case temp of
                    Filled _ -> temp
                    Empty domain -> 
                        if i == i' || j == j' || (i `div` 3, j `div` 3) == (i' `div` 3, j' `div` 3)
                            then clearDom temp val
                            else temp
    ]


cutTheRows :: [Cell] -> [[Cell]]
cutTheRows [] = []
cutTheRows row = let (puzzle, rest) = splitAt 9 row in puzzle : cutTheRows rest

updateGrid :: Grid -> (Loc, Int) -> Grid
updateGrid grid (loc, val) = cutTheRows $ updateRelative grid (loc, val)

solveGrid :: Grid -> [Grid]
solveGrid grid =
    case mrv grid of
        Nothing -> [grid]
        Just (length, domain, (i, j)) ->
            concat [ solveGrid (updateGrid grid ((i, j), v)) | v <- domain ]

getSolution :: Grid -> Grid
getSolution = head . solveGrid . filterGrid

rowToText :: [Cell] -> String
rowToText row = unwords $ map (\case Filled v -> show v; Empty _ -> ".") row

gridToText :: Grid -> String
gridToText grid = unlines $ map rowToText grid

solve' :: [String] -> String
solve' [] = ""
solve' (id:grid) =
    let grid' = convertToGrid grid
        solution = getSolution grid'
    in
        id ++ "\n" ++ gridToText solution

solve :: [[String]] -> String
solve = unlines . map solve'

main :: IO ()
main = do
    file <- readFile "sudoku.txt"
    let grids = splitFileToGrids $ lines file
    writeFile "solutions.txt" $ solve grids

-- main :: IO ()
-- main = do
--     file <- readFile "sudoku.txt"
--     let grids = splitFileToGrids $ lines file
--     putStrLn (solve grids)

