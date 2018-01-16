-- Provides data type and methods for manipulating riddle board field.
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

-- FieldContainer provide common methods for querying content of FieldMap.
-- Query methods implementation rely on getFields method must be implemented by
-- instances.
class FieldContainer a
  where
    getFields :: a -> FieldMap
    filterFields :: Field -> a -> FieldMap
    filterFields f c = M.filter (==f) (getFields c)
    countFields :: Field -> a -> Int
    countFields f c = M.size $ filterFields f c
    countTanks :: a -> Int
    countTanks c = countTanksUp + countTanksDown + countTanksLeft + countTanksRight
      where
        countTanksUp = countFields TankUp c
        countTanksDown = countFields TankDown c
        countTanksLeft = countFields TankLeft c
        countTanksRight = countFields TankRight c
    findFields :: Field -> a -> FieldAssociationList
    findFields f c = M.toList $ filterFields f c
    fieldEmpty :: (Int, Int) -> a -> Bool
    fieldEmpty k c = M.notMember k (getFields c)

-- Data type representing riddle board field.
-- The Grass constructor represents field which can't contain Tank.
data Field = Grass | House | TankUp | TankDown | TankLeft | TankRight deriving (Eq, Show)

instance Renderable Field
  -- need to run "chcp.com 65001" in cmd to enable showing unicode symbols
  where
    render field = case field of
        Grass -> "#"
        --House -> "⌂"
        House -> "H"
        --Tank -> "O"
        TankUp -> "^"
        TankDown -> "v"
        TankLeft -> "<"
        TankRight -> ">"
transposeMap :: M.Map (Int, Int) a -> M.Map (Int, Int) a
transposeMap = M.mapKeys swap