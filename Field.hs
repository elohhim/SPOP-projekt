module Field 
( Field(..)
, FieldMap
, FieldAssociationList
, FieldContainer(..)
, transposeMap) where

import Rendering
import Data.Tuple (swap)
import qualified Data.Map as M

type FieldMap = M.Map (Int, Int) Field
type FieldAssociationList = [((Int, Int), Field)]

class FieldContainer a
  where
    getFields :: a -> FieldMap
    filterFields :: Field -> a -> FieldMap
    filterFields f c = M.filter (==f) (getFields c)
    countFields :: Field -> a -> Int
    countFields f c = M.size $ filterFields f c
    findFields :: Field -> a -> FieldAssociationList
    findFields f c = M.toList $ filterFields f c
    fieldEmpty :: (Int, Int) -> a -> Bool
    fieldEmpty k c = M.notMember k (getFields c)

data Field = Grass | House | Tank deriving (Eq, Show)

instance Renderable Field
  -- need to run "chcp.com 65001" in cmd to enable showing unicode symbols
  where
    render field = case field of
        Grass -> "#"
        House -> "⌂"
        Tank -> "O"
transposeMap :: M.Map (Int, Int) a -> M.Map (Int, Int) a
transposeMap = M.mapKeys swap