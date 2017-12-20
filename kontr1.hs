module Kontrolno where

import Data.List
import Control.Monad

middle1 [] = "-1"
middle1 [x] = [x]
middle1 l = middle1 $ init $ tail l
middleDigit :: Int -> Int
middleDigit = read . middle1 . show

middle2 [x] = "-1"
middle2 [x,y] = [x,y]
middle2 l = middle2 $ init $ tail l
middleDigits :: Int -> Int
middleDigits = read . middle2 . show

-- join f x = f x x
countCols = length . filter (join $ elem . (/2) . sum) . transpose

-- liftM2 h f g x = h (f x) (g x)
twoRows  = liftM2 (>) (maximum . fst) (minimum . snd) . unzip .
           map (liftM2 (,) minimum maximum)

mapsTo f l1 l2 = all ((`elem` l2) . f) l1
-- mapsTo = flip . (all .) . flip ((.) . flip elem)
isEm l op f = mapsTo f l l && and [ f x` op` f y == f (x `op` y) | x <- l, y <- l ]

isSur l1 l2 f = mapsTo f l1 l2 && all ((`any` l1) . (. f) . (==)) l2

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Eq,Show)
leaf = (`join` Empty) . Node
root (Node x _ _) = x

inTree _ Empty = False
inTree x (Node y l r) = x == y || inTree x l || inTree x r

nodes Empty = 0
nodes (Node x l r) = 1 + nodes l + nodes r

cousins _ Empty = 0
cousins x (Node _ l r)
  | root l == x || root r == x    = 0
  | inTree x l                    = cousins x l + succs r
  | inTree x r                    = succs l + cousins x r
  | otherwise                     = 0
   where succs t = max 0 (nodes t - 1)

cut Empty        = Empty
cut (Node x _ _) = leaf x

family (Node x l r) = Node x (cut l) (cut r)

families Empty          = []
families t@(Node _ l r) = family t:families l ++ families r

hasDuplicates []     = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

familiesAlike :: (Eq a) => BinaryTree a -> Bool
familiesAlike = hasDuplicates . families

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
  isEm [0,1,4,6] (+) (`mod` 3),
  isSur [0,1,-1,2] [0,1,4] (^2),
  cousins 0 testTree1 == 8,
  cousins 1 testTree1 == 6,
  cousins 3 testTree1 == 0,
  not $ familiesAlike testTree1,
  familiesAlike testTree2
  ]
  
