module Data.Array.Utils.Internal where

import Data.Array

genTableArray :: Int -> Int -> ((Int, Int) -> a) -> Array (Int, Int) a
genTableArray w h f = array ((0, 0), (w-1, h-1)) (zip indexes (map f indexes)) where
    indexes = getIndexes w h

getIndexes :: Int -> Int -> [(Int, Int)]
getIndexes w h = let
    xs = concat (replicate h [0..(w-1)])
    ys = concatMap (replicate w) [0..(h-1)]
    in zip xs ys

mapArray :: Array (Int, Int) a -> ((Int, Int) -> a -> a) -> Array (Int, Int) a
mapArray a f = let
    is = indices a
    es = elems a
    as = zip is (zipWith f is es)
    in a // as