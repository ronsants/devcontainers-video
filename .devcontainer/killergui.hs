{-# LANGUAGE OverloadedStrings #-}
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
-- import KillerSudoku
import Data.Text (pack)

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
  -- Set up the window
  set UI.title "Killer Sudoku Solver" $ return window

  -- Grid display (9x9)
  cells <- mapM (\(x, y) -> UI.input # set UI.size "2" # set (UI.attr "id") (show x ++ show y)) 
           [(x, y) | x <- [0..8], y <- [0..8]]
  let gridUI = grid $ chunksOf 9 cells

  -- Solve button
  solveButton <- UI.button #+ [string "Solve"]

  -- Cage input (simplified: hardcoded for now)
  cageInput <- UI.input # set UI.placeholder "Enter cages (e.g., 7:00,01;8:10,11)"

  -- Layout
  getBody window #+ [UI.div #+ [gridUI], element cageInput, element solveButton]

  -- Event handling
  on UI.click solveButton $ \_ -> do
    -- Hardcoded example cages for now
    let cages = [Cage [(0,0), (0,1)] 7, Cage [(1,0), (1,1)] 8, 
                 Cage [(0,2), (1,2)] 5] -- Extend to 9x9
        initialGrid = []
    case solve cages initialGrid of
      (solution:_) -> mapM_ (\((x, y), v) -> 
        element (cells !! (x * 9 + y)) # set UI.value (show v)) solution
      _ -> liftIO $ putStrLn "No solution found"

-- Helper to split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = if null xs then [] else take n xs : chunksOf n (drop n xs)