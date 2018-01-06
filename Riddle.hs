module Riddle (Riddle (..), makeRiddle, renderRiddle, RowDef, ColDef, HouseDef) where

import Board
 
type RowDef = [Int]
type ColDef = [Int]
type HouseDef = [(Int, Int)]
data Riddle = Riddle { rowDef :: RowDef
                     , colDef :: ColDef
                     , board :: Board
                     } deriving (Show)

makeRiddle :: (RowDef, ColDef, HouseDef) -> Riddle
makeRiddle (rdef, cdef, hdef) = Riddle rdef cdef board where
  size = (length rdef, length cdef)
  board = makeBoard size (zip hdef (repeat House))
  
renderRiddle :: Riddle -> String
renderRiddle (Riddle rd cd b) = unlines (colLine:rowLines)
  where
    colLine = "  " ++ concat (map show cd)
    rowLines = map rowLine (zip rd boardLines)
      where
        boardLines = lines $ renderBoard b
        rowLine (rowDef, boardLine) = show rowDef ++ " " ++ boardLine