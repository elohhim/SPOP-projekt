module Riddle 
( Riddle (..)
, makeRiddle
, solve) where

import Board
import Field
import Rendering

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

                     
makeRiddle :: ([Int], [Int], [(Int, Int)]) -> Riddle
makeRiddle (rdef, cdef, hdef) = Riddle rdef cdef board 
  where
    board = makeBoard rows cols (zip hdef (repeat House))
    rows = length rdef
    cols = length cdef

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
    solve'' riddle hs (p:ps) = if isStateValid newState 
                               then solve' newState hs
                               else solve'' riddle hs ps
      where
        newState = newState' riddle
        newState' (Riddle rdef cdef board) = Riddle rdef cdef newBoard
          where
            newBoard = buildTank p board
    
    
isStateValid :: Riddle -> Bool
isStateValid riddle = rowsValid riddle && rowsValid (transposeRiddle riddle)
  where
    rowsValid (Riddle rdef _ board) =
      let zipRowsWithDef = zip (asRows board) rdef
      in  and $ [rowValid r | r <- zipRowsWithDef]
    rowValid (row, tankCount) = countFields Tank row <= tankCount
    
transposeRiddle :: Riddle -> Riddle
transposeRiddle (Riddle rdef cdef b) = Riddle cdef rdef (transposeBoard b)