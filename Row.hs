-- Provides data type and methods for manipulating riddle board  as rows
module Row 
( Row(..)
) where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)

import Field
import Rendering

-- Data type representing row of board
data Row = Row { number :: Int
               , size :: Int
               , fields :: FieldMap
               } deriving (Show)

instance Renderable Row
  where
    render row = concat $ map (maybe "\x25A1" render) (asList row)
    
instance FieldContainer Row
  where
    getFields = fields

-- Transform Row into explicit list of Maybe Field where Nothing means empty
-- field. This representation is more convenient in some cases.
asList :: Row -> [Maybe Field]
asList (Row n len fields) = [M.lookup (n, col) fields | col <- [0..len-1]]

-- Opposite to asList.
fromList :: Int -> [Maybe Field] -> Row
fromList n fs = Row n (length fs) fieldMap
  where
    fieldMap = M.fromList $ mapMaybe assocciation (zip [0..] fs)
    assocciation (c, Just f) = Just ((n,c), f)
    assocciation (_, Nothing) = Nothing