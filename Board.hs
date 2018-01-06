module Board 
(Board (..), Field (..), makeBoard, renderBoard) where

import qualified Data.Map as Map

type Coord = (Int, Int)
data Field = Grass | House | Tank deriving (Eq, Show)
type BoardRow = [Maybe Field] -- maybe BoardRow {len :: Int, fields :: Map.Map Int Field}
type BoardColumn = [Maybe Field]
type FieldMap = Map.Map Coord Field
data Board = Board {size :: Coord, fields :: FieldMap} deriving (Show)

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

getRows :: Board -> [BoardRow]
getRows board = [getRow n board | n <- [0..(fst $ size board)]]

renderBoard :: Board -> String
renderBoard board = unlines (map renderRow (getRows board))

renderRow :: BoardRow -> String
renderRow row = concat (map (maybe "#" renderField) row)

renderField :: Field -> String
renderField Grass = "x"
renderField House = "^"
renderField Tank = "O"