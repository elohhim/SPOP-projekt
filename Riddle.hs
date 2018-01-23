-- Provides data type and methods for creating and solving riddle
module Riddle 
( Riddle (..)
, makeRiddle
, validateDefinition
, solve
, solve2) where

--import qualified Data.Map as M
import Data.List (intersect)

import Board
import Field
import Rendering
import qualified Row as R

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

-- Method validating given definition, purely for our convenience when testing
validateDefinition :: ([Int], [Int], [(Int, Int)]) -> Bool
validateDefinition (rdef, cdef, hdef) = rowColEq && houseTankEq where
  rowColEq = sum rdef == sum cdef
  houseTankEq = sum rdef == length hdef

        
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
solve riddle = solve' griddle houses
  where
    griddle = addGrassIfNoHouse (addGrass riddle) houses
    houses = map fst (findFields House (board riddle))
    solve' :: Riddle -> [(Int, Int)] -> Riddle
    solve' riddle [] = riddle
    solve' riddle (h:hs) = solve'' riddle hs emptyParcels h
      where
        emptyParcels = adjacentCoordEmpty h (board riddle)
    solve'' riddle _ [] _ = riddle
    solve'' riddle hs (p:ps) h = if isSolved solution
                                 then solution
                                 else solve'' riddle hs ps h
      where
        solution = solve' newState hs
        newState = newState' riddle
        newState' (Riddle rdef cdef board) = addGrassIfNoHouse (addGrass newRiddle) hs
          where
            newRiddle = Riddle rdef cdef newBoard
            newBoard = buildTank p h board


-- Second version of algorithm.
-- 1(solve'). Check if there are any fields in which must be a tank, 
--   because the number of empty fields in a row is equal to the remaining number of tanks.
--   If yes go to 3, otherwise go to 2.
-- 2(solve''). Build tanks house by house like in the first algorithm. After each house go to 1.
-- 3(solve'''). Get possible houses for the first field in which must be a tank. Go to 4.
--   If there are no more 'must' fields go to 1.
-- 4(solve''''). Build tanks for 'must' fields step by step. 
--   If a valid solution is not obtained algorithm steps back to
--   previous step processing another house next to a 'must' field.
solve2 :: Riddle -> Riddle
solve2 riddle = solve' griddle houses
  where
    griddle = addGrassIfNoHouse (addGrass riddle) houses
    houses = map fst (findFields House (board riddle))
    solve' :: Riddle -> [(Int, Int)] -> Riddle
    solve' riddle [] = riddle
    solve' riddle (h:hs) = if mustParcels == []
                           then solve'' riddle hs emptyParcels h
                           else solve''' riddle (h:hs) mustParcels
      where
        mustParcels = getMustCoords (board riddle) (rowDef riddle) (colDef riddle)
        emptyParcels = adjacentCoordEmpty h (board riddle)

    solve'' riddle _ [] _ = riddle
    solve'' riddle hs (p:ps) h = if isSolved solution
                                 then solution
                                 else solve'' riddle hs ps h
      where
        solution = solve' newState hs
        newState = newState' riddle
        newState' (Riddle rdef cdef board) = addGrassIfNoHouse (addGrass newRiddle) hs
          where
            newRiddle = Riddle rdef cdef newBoard
            newBoard = buildTank p h board

    solve''' riddle [] _ = riddle
    solve''' riddle hs [] = solve' riddle hs
    solve''' riddle hs (m:ms) = solve'''' riddle m mhouses hs ms
      where
        mhouses = adjacentCoordHouse m (board riddle)
    solve'''' riddle _ [] _ _ = riddle
    solve'''' riddle m (mh:mhs) hs ms = if elem mh hs && isSolved solution
                                        then solution
                                        else solve'''' riddle m mhs hs ms
      where
        solution = solve''' newState newHouses ms
        newState = newState' riddle
        newState' (Riddle rdef cdef board) = addGrassIfNoHouse (addGrass newRiddle) newHouses
          where
            newRiddle = Riddle rdef cdef newBoard
            newBoard = buildTank m mh board
        newHouses = filter (/= mh) hs


-- Checks if current Riddle state is valid            
isStateValid :: Riddle -> Bool
isStateValid riddle = rowsValid riddle && rowsValid (transposeRiddle riddle)
  where
    rowsValid (Riddle rdef _ board) =
      let zipRowsWithDef = zip (asRows board) rdef
      in  and $ [rowValid r | r <- zipRowsWithDef]
    -- row is considered 
    rowValid (row, tankCount) = countTanks row <= tankCount


-- Checks if current Riddle is solved. We don't need to check if every House is
-- connected to Tank as our algorithm is not producing such states.
isSolved :: Riddle -> Bool
isSolved riddle = rowsSolved riddle && rowsSolved (transposeRiddle riddle)
  where
    rowsSolved (Riddle rdef _ board) =
      let zipRowsWithDef = zip (asRows board) rdef
      in  and $ [rowSolved r | r <- zipRowsWithDef]
    -- we consider Row solved when it contain exact amount of Tanks as in definition
    rowSolved (row, tankCount) = countTanks row == tankCount


-- We use rows when analyzing riddle so to operate on columns way of transposing
-- whole riddle is needed.    
transposeRiddle :: Riddle -> Riddle
transposeRiddle (Riddle rdef cdef b) = Riddle cdef rdef (transposeBoard b)


-- Set to Grass all fields in rows and columns that already satisfy number of tanks in them.
addGrass :: Riddle -> Riddle
addGrass riddle = addGrass' triddle
  where
    triddle = transposeRiddle (addGrass' (transposeRiddle riddle))
    addGrass' (Riddle rdef cdef board) = Riddle rdef cdef newBoard
      where
        newBoard = fromRows (addGrassRows (asRows board) rdef)
          where
            addGrassRows [] _ = []
            addGrassRows (r:rs) (rd:rds) = if rowSolved r rd
                                           then (R.plantGrass r) : (addGrassRows rs rds)
                                           else r : (addGrassRows rs rds)
            rowSolved row tankCount = countTanks row == tankCount


-- Set to Grass all empty fields which are not adjacent to remaining houses.
addGrassIfNoHouse :: Riddle -> [(Int, Int)] -> Riddle
addGrassIfNoHouse riddle [] = riddle
addGrassIfNoHouse riddle houses = addGrassIfNoHouse' riddle emptyFields
  where
    emptyFields = filter (`fieldEmpty` (board riddle)) coords
    coords = [(x,y)| x<-[0..rows-1], y<-[0..cols-1]]
    rows = length (rowDef riddle)
    cols = length (colDef riddle)
    addGrassIfNoHouse' riddle [] = riddle
    addGrassIfNoHouse' (Riddle rdef cdef board) fields = Riddle rdef cdef newBoard
      where
        newBoard = setFields (noHouseFields fields board) Grass board
        noHouseFields [] _ = []
        noHouseFields (f:fs) board = if (intersect adjacent houses) == []
                                     then f : noHouseFields fs board
                                     else noHouseFields fs board
          where
            adjacent = adjacentCoordHouse f board