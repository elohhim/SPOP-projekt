module Row 
( Row(..)
) where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)

import Field
import Rendering

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
    
asList :: Row -> [Maybe Field]
asList (Row n len fields) = [M.lookup (n, col) fields | col <- [0..len-1]]

fromList :: Int -> [Maybe Field] -> Row
fromList n fs = Row n (length fs) fieldMap
  where
    fieldMap = M.fromList $ mapMaybe assocciation (zip [0..] fs)
    assocciation (c, Just f) = Just ((n,c), f)
    assocciation (_, Nothing) = Nothing