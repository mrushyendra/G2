module Sorting where

import qualified Data.Map as M
import qualified Data.List as L

g2Entry :: Int -> Int
g2Entry a = maximum (map (+1) [1, 2, a, 4, 5])

g2Entry2 :: Int -> Int
g2Entry2 a = minimum [1, 2, 3]


g2Entry3 :: Int -> Int
g2Entry3 a = foldr (+) 0 [1, a, 3]

g2Entry4 :: Int -> Int
g2Entry4 a = foldr (+) 4 [1, a, 3, 4, 5]

g2Entry5 :: [Int] -> Int
g2Entry5 xs = head $ tail xs

g2Entry6 :: Int -> Int
g2Entry6 a = let m = M.fromList [(1, 'a'), (2, 'b')]
                 m' = M.insert a 'c' m
             in case M.lookup 1 m' of
               Just _ -> 13579
               _ -> 24680

fromList22 :: [(Int, Float)] -> [(Int, Float)]
fromList22 = foldr (:) []

fromList23 :: [Int] -> [Int]
fromList23 = foldr (:) []

foldrx :: (a -> b -> b) -> b -> [a] -> b
foldrx k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys
g2Entry7 :: Int -> [(Int, Int)]
g2Entry7 a = let m = M.fromList [(123456, a)]
             in M.toList m

g2Entry8 :: [(Int, Float)] -> M.Map Int Float
g2Entry8 = M.fromList
