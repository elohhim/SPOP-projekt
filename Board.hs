module Board 
(Board (..), Field (..), makeBoard, render, asRows, fromRows) where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Rendering

type Coord = (Int, Int)
data Field = CrossedOut | House | Tank deriving (Eq, Show)

instance Renderable Field
  where
    render CrossedOut = "x"
    render House = "^"
    render Tank = "O"
    
type BoardRow = [Maybe Field]
type BoardColumn = [Maybe Field]
type FieldMap = Map.Map Coord Field
data Board = Board {size :: Coord, fields :: FieldMap} deriving (Show)

instance Renderable Board
  where
    render board = unlines (map renderRow (asRows board))
      where
        renderRow row = concat (map (maybe "#" render) row)

makeBoard :: Coord -> [(Coord, Field)] -> Board
makeBoard size fields = Board size (Map.fromList fields)

fieldsInRow :: Int -> FieldMap -> FieldMap
fieldsInRow row = Map.filterWithKey (\k _ -> (fst k) == row)

fieldsInColumn :: Int -> FieldMap -> FieldMap
fieldsInColumn col = Map.filterWithKey (\k _ -> (snd k) == col)

getRow :: Int -> Board -> BoardRow
getRow n (Board (rows, cols) fields) = getRow' cols (fieldsInRow n fields)
  where
    getRow' len fields = [Map.lookup (n, col) fields | col <- [0..len-1]]

asRows :: Board -> [BoardRow]
asRows board = [getRow n board | n <- [0..(fst $ size board)]]

fromRows :: [BoardRow] -> Board
fromRows rows = Board size fieldMap
  where
    size = (length rows, length $ head rows)
    fieldMap = Map.fromList rowsFields
    rowsFields = [((r, c), f) | (r, row) <- idxRows, (c, f) <- idxFields row]
    idxRows = zip [0..] rows 
    idxFields row = mapMaybe filterField (zip [0..] row)
      where
        filterField (_, Nothing) = Nothing
        filterField (c, Just f) = Just (c, f)