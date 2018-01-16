{-# LANGUAGE DeriveFoldable #-}
module MidtermTest where

import Data.List (transpose, nub)
import Control.Monad (join, liftM2)
import Data.Function (on)

middle1 []  = "-1"
middle1 [x] = [x]
middle1 l   = middle1 $ init $ tail l
middleDigit :: Int -> Int
middleDigit = read . middle1 . show

middle2 [x]   = "-1"
middle2 [x,y] = [x,y]
middle2 l     = middle2 $ init $ tail l
middleDigits :: Int -> Int
middleDigits = read . middle2 . show

-- (f =<< g) x = f (g x) x
countCols = length . filter (elem =<< (/2) . sum) . transpose

-- liftM2 h f g x = h (f x) (g x)
twoRows :: (Ord a) => [[a]] -> Bool
twoRows  = liftM2 (>) (maximum . map minimum) (minimum . map maximum)

-- (f .* g) x y = f (g x y)
(.*) = (.) . (.)

subset :: (Eq a) => [a] -> [a] -> Bool
subset = all . flip elem

mapsTo, mapsFrom :: (Eq a) => (a -> a) -> [a] -> [a] -> Bool
mapsTo = flip subset .* map
mapsFrom = subset .* map

-- liftM2_2 h f g x y = h (f x y) (g x y)
liftM2_2 :: (a -> b -> c) -> (d -> e -> a) -> (d -> e -> b) -> d -> e -> c
liftM2_2 = liftM2 . liftM2

-- liftM2_3 h f g x y z = h (f x y z) (g x y z)
liftM2_3 :: (a -> b -> c) -> (d -> e -> f -> a) -> (d -> e -> f -> b) -> d -> e -> f -> c
liftM2_3 = liftM2 . liftM2_2

isEm f l op = mapsTo f l l && and [ f x `op` f y == f (x `op` y) | x <- l, y <- l ]
                           -- ((`all` l) . liftM2_2 (==) (op `on` f) (f .* op)) `all` l

isSur :: (Eq a) => (a -> a) -> [a] -> [a] -> Bool
isSur = liftM2_3 (&&) mapsTo mapsFrom

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Eq,Show,Foldable)

root = foldr (const . Just) Nothing

cousins _ Empty = 0
cousins x (Node _ l r)
  | root l == Just x   = 0
  | root r == Just x   = 0
  | x `elem` l         = cousins x l + succs r
  | x `elem` r         = succs l + cousins x r
  | otherwise          = 0
   where succs = max 0 . subtract 1 . length

cut = foldr (const . leaf) Empty

family (Node x l r) = Node x (cut l) (cut r)

families Empty          = []
families t@(Node _ l r) = family t:families l ++ families r

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates = (/=) =<< nub

familiesAlike :: Eq a => BinaryTree a -> Bool
familiesAlike = hasDuplicates . families

-- join f x = f x x
leaf = (`join` Empty) . Node

genTree x y
 | x == y    = leaf x
 | otherwise = Node mid (genTree x (mid - 1)) (genTree (mid + 1) y)
  where mid = (x + y) `div` 2
testTree1 = genTree 0 14

testTree2 = Node 1 (Node 2 (Node 3 (leaf 5) Empty) (leaf 4))
                   (Node 6 (Node 2 (leaf 3) (Node 4 Empty (leaf 7))) Empty)


tests = and [
  middleDigit 452 == 5,
  middleDigit 4712 == -1,
  middleDigits 452 == -1,
  middleDigits 4712 == 71,
  countCols [[1,2,3,6],[2,3,4,2],[3,4,5,4]] == 2,
  twoRows   [[1,2,3],[2,3,4],[3,4,5],[6,5,4]],
  isEm (`mod` 3) [0,1,4,6] (+),
  isSur (^2) [0,1,-1,2] [0,1,4],
  cousins 0 testTree1 == 8,
  cousins 1 testTree1 == 6,
  cousins 3 testTree1 == 0,
  not $ familiesAlike testTree1,
  familiesAlike testTree2
  ]
