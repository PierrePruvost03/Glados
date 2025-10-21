module DataStruct.Bytecode.Utils (
    construct,
    constructList,
    putManyMany,
    getMany,
    getList
) where

import Data.Binary

construct :: Binary t => (t -> d) -> Get d
construct f = get >>= \x -> return (f x)

constructList :: Binary t => ([t] -> d) -> Get d
constructList f = getList get >>= \x -> return (f x)

putManyMany :: Binary t => [[t]] -> Put
putManyMany l = put (length l) <> f l
    where
        f [] = mempty
        f (x:xs) = putList x <> f xs

getMany :: (Get a) -> Int -> Get [a]
getMany _ 0 = return []
getMany g x = (:) <$> g <*> getMany g (x - 1)

getList :: (Get a) -> Get [a]
getList g = (get :: Get Int) >>= \x -> getMany g x
