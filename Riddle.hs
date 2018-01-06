module Riddle (Riddle (..), makeRiddle, render, RowDef, ColDef, HouseDef, solveRiddle) where

import Board
import Rendering

type RowDef = [Int]
type ColDef = [Int]
type HouseDef = [(Int, Int)]
data Riddle = Riddle { rowDef :: RowDef
                     , colDef :: ColDef
                     , board :: Board
                     } deriving (Show)

instance Renderable Riddle
  where
    render (Riddle rd cd b) = unlines (colLine:rowLines)
      where
        colLine = "  " ++ concat (map show cd)
        rowLines = map rowLine (zip rd boardLines)
        boardLines = lines $ render b
        rowLine (rowDef, boardLine) = show rowDef ++ " " ++ boardLine

                     
makeRiddle :: (RowDef, ColDef, HouseDef) -> Riddle
makeRiddle (rdef, cdef, hdef) = Riddle rdef cdef board where
  size = (length rdef, length cdef)
  board = makeBoard size (zip hdef (repeat House))

solveRiddle :: Riddle -> Riddle
solveRiddle (Riddle rdef cdef board) = Riddle rdef cdef board'
  where
    board' = fromRows $ asRows board 