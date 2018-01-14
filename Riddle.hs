-- Provides data type and methods for creating and solving riddle
module Riddle 
( Riddle (..)
, makeRiddle
, solve) where

import Board
import Field
import Rendering

-- Data type representing architects riddle
data Riddle = Riddle { rowDef :: [Int]
                     , colDef :: [Int]
                     , board :: Board
                     } deriving (Show)

instance Renderable Riddle
  where
    render (Riddle rd cd b) = unlines (colLine:rowLines)
      where
        colLine = "  " ++ concat (map show cd)
        rowLines = map (rowLine) (zip rd boardLines)
        boardLines = map render (asRows b)
        rowLine (rowDef, boardLine) = show rowDef ++ " " ++ boardLine

-- Factory method for crating Riddle object from definition
makeRiddle :: ([Int], [Int], [(Int, Int)]) -> Riddle
makeRiddle (rdef, cdef, hdef) = Riddle rdef cdef board 
  where
    board = makeBoard rows cols (zip hdef (repeat House))
    rows = length rdef
    cols = length cdef

-- Function which solves given riddle.
-- In each step algorithm tries to build tank for one of the houses on one of
-- the empty adjacent field. It tries to fill board with tanks for each house
-- step by step. If the resulting riddle board is valid solution it is returned
-- otherwise algorithm tries next empty field adjacent to house processed in
-- current step. If there is no available fields left algorithm steps back to
-- previous step processing another house.
solve :: Riddle -> Riddle
solve riddle = solve' riddle houses
  where
    houses = map fst (findFields House (board riddle))
    solve' :: Riddle -> [(Int, Int)] -> Riddle
    solve' riddle [] = riddle
    solve' riddle (h:hs) = solve'' riddle hs emptyParcels
      where
        emptyParcels = adjacentCoordEmpty h (board riddle)
    solve'' riddle _ [] = riddle
    solve'' riddle hs (p:ps) = if isSolved solution
                               then solution
                               else solve'' riddle hs ps
      where
        solution = solve' newState hs
        newState = newState' riddle
        newState' (Riddle rdef cdef board) = Riddle rdef cdef newBoard
          where
            newBoard = buildTank p board

-- Checks if current Riddle state is valid            
isStateValid :: Riddle -> Bool
isStateValid riddle = rowsValid riddle && rowsValid (transposeRiddle riddle)
  where
    rowsValid (Riddle rdef _ board) =
      let zipRowsWithDef = zip (asRows board) rdef
      in  and $ [rowValid r | r <- zipRowsWithDef]
    -- row is considered 
    rowValid (row, tankCount) = countFields Tank row <= tankCount

-- Checks if current Riddle is solved. We don't need to check if every House is
-- connected to Tank as our algorithm is not producing such states.
isSolved :: Riddle -> Bool
isSolved riddle = rowsSolved riddle && rowsSolved (transposeRiddle riddle)
  where
    rowsSolved (Riddle rdef _ board) =
      let zipRowsWithDef = zip (asRows board) rdef
      in  and $ [rowSolved r | r <- zipRowsWithDef]
    -- we consider Row solved when it contain exact amount of Tanks as in definition
    rowSolved (row, tankCount) = countFields Tank row == tankCount

-- We use rows when analyzing riddle so to operate on columns way of transposing
-- whole riddle is needed.    
transposeRiddle :: Riddle -> Riddle
transposeRiddle (Riddle rdef cdef b) = Riddle cdef rdef (transposeBoard b)