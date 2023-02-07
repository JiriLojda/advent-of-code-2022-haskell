module Day17.CyclicList (readItem, create, CyclicList) where

newtype CyclicList a = MkCyclicList ([a], [a]) deriving Show -- (rest items to read, read items) both non-empty

create :: [a] -> Either String (CyclicList a)
create [] = Left "Tried to create empty cyclic list. That is not possible."
create lst = Right (MkCyclicList (lst, []))

readItem :: CyclicList a -> (a, CyclicList a)
readItem (MkCyclicList (item : restToRead, readItems)) = (item, MkCyclicList (restToRead, item : readItems))
readItem lst = readItem (reset lst)

reset :: CyclicList a -> CyclicList a
reset (MkCyclicList (itemsToRead, readItems)) = MkCyclicList (itemsToRead ++ reverse readItems, [])
