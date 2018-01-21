{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Types where

import Control.Monad
import Data.Function

average l = sum l / fromIntegral (length l)

class Measurable a where
  size :: a -> Int
  empty :: a -> Bool
  empty  = (==0) . size

larger :: Measurable a => a -> a -> Bool
--larger x y = size x > size y
-- larger x y = (>) (size x) (size y)
-- on f g x y = f (g x) (g y)
larger = (>) `on` size

instance Measurable Integer where
  size 0 = 0
  size n = 1 + size (n `div` 10)
--  empty = (==0)

instance (Measurable a, Measurable b) => Measurable (a, b) where
  size (x, y) = size x + size y

instance (Measurable a) => Measurable [a] where
  size = sum . map size

-- type Player = (String, Int) -- играч с име и резултат


--data (Measurable a) => Player a = P String a

-- pesho :: Player Integer
-- pesho = ("Пешо", 12)
-- pesho = P "Пешо" 12

-- data Player = Player Name Score
data Player = Player { name :: Name, score :: Score }
  deriving (Eq, Ord, Read, Show)
type Name = String
type Score = Int

katniss :: Player
-- katniss = Player "Katniss Everdeen" 45
katniss = Player { score = 45, name = "Katniss Everdeen" }
mario = Player { score = 55, name = "Mario" }

-- katniss = Player("Katniss Everdeen", 45")

better :: Player -> Player -> Player
better p1@(Player {score = score1}) p2@(Player {score = score2})
  | score1 > score2 = p1
  | otherwise       = p2

type Matrix a = [[a]]

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read, Enum)

getAt :: [a] -> Int -> Maybe a
getAt []     _ = Nothing
getAt (x:_)  0 = Just x
getAt (_:xs) n = getAt xs (n - 1)

data ScoreOrPlayers = Score Int | Players [Player]
  deriving Show

{-
bestScore :: [Player] -> Either Int [Player]
bestScore players
  | length bestPlayers == 1 = Left bestScore
  | otherwise               = Right bestPlayers
  where bestScore   = maximum $ map score players
        bestPlayers = filter ((bestScore==) . score) players
-}

bestScore :: [Player] -> ScoreOrPlayers
bestScore players
  | length bestPlayers == 1 = Score best
  | otherwise               = Players bestPlayers
  where best        = maximum $ map score players
        bestPlayers = filter ((==best) . score) players

getBestScore :: [Player] -> Maybe Int
getBestScore players =
  case best of
    (Score x) -> Just x
    _         -> Nothing
  where best = bestScore players

data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show, Read)

five = Succ $ Succ $ Succ $ Succ $ Succ Zero

toNat :: Int -> Nat
toNat 0 = Zero
toNat n = Succ $ toNat $ n - 1

fromNat :: Nat -> Int
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n

data Bin = One | BitZero Bin | BitOne Bin
  deriving (Eq, Ord, Show, Read)

-- 110
six = BitZero $ BitOne $ One

fromBin :: Bin -> Int
fromBin One         = 1
fromBin (BitZero b) = 2 * fromBin b
fromBin (BitOne  b) = 2 * fromBin b + 1

toBin :: Int -> Bin
toBin 1 = One
toBin n
 | even n    = BitZero half
 | otherwise = BitOne  half
  where half = toBin (n `div` 2)

-- 11 + 1
succBin :: Bin -> Bin
succBin One         = BitZero One
succBin (BitZero b) = BitOne b
succBin (BitOne  b) = BitZero $ succBin b

data List a = Nil | Cons { listHead :: a, listTail :: List a }
  deriving (Eq, Ord, Show, Read)

{-
data [] a = [] | (:) a ([] a)
-}

l = Cons 1 $ Cons 2 $ Cons 3 Nil

fromList :: [a] -> List a
fromList []     = Nil
fromList (x:xs) = Cons x $ fromList xs

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

Nil         +++ l = l
(Cons x xs) +++ l = Cons x (xs +++ l)

-- data BinTree a = Empty | Node a (BinTree a) (BinTree a)
data BinTree a = Empty | Node { root  :: a,
                                left  :: BinTree a,
                                right :: BinTree a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable)
leaf x = Node x Empty Empty

t = Node 3 (leaf 1) (leaf 5)
depth :: BinTree a -> Int
depth Empty        = 0
-- depth (Node _ l r) = 1 + max (depth l) (depth r)
depth (Node _ l r) = 1 + (max `on` depth) l r

leaves :: BinTree a -> [a]
leaves (Node x Empty Empty) = [x]
leaves (Node _ l     r    ) = leaves l ++ leaves r

grow :: BinTree a -> a -> BinTree a
grow (Node x Empty Empty) y = Node x (leaf y) (leaf y)
grow (Node x l     r    ) y = Node x (grow l y) (grow r y)

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree _ Empty        = Empty
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

clone :: Num a => BinTree a -> a -> a -> BinTree a
clone t x y = Node x (mapTree (+y) t) (mapTree (+y) t)

foldrTree :: (a -> b -> b) -> b -> BinTree a -> b
foldrTree _  nv Empty        = nv
foldrTree op nv (Node x l r) = foldrTree op (x `op` foldrTree op nv r) l

-- b = [a]
nodes = foldrTree (:) []
countNodes = foldrTree (\_ x -> x + 1) 0

{-
instance Eq a => Eq (BinTree a) where
  Empty == Empty = True
  (Node x1 l1 r1) == (Node x2 l2 r2) = x1 == x2 && l1 == l2 && r1 == r2
  _ == _ = False
-}

-- data Tree a = Tree { root :: a, subtrees :: [Tree a] }
data Tree a     = Tree { troot :: a, subtrees :: TreeList a }
  deriving (Eq, Ord, Show, Read)
data TreeList a = None |
                  SubTree { firstTree :: Tree a, restTrees :: TreeList a }
  deriving (Eq, Ord, Show, Read)

tleaf x = Tree x None

tree = Tree 1 $ SubTree (tleaf 2)
              $ SubTree (Tree 3 (SubTree (tleaf 4) None))
              $ SubTree (tleaf 5) None

level :: Tree a -> Int -> [a]
level (Tree x _ ) 0 = [x]
level (Tree _ ts) n = levelTrees ts (n - 1)

levelTrees :: TreeList a -> Int -> [a]
levelTrees None           _ = []
levelTrees (SubTree t ts) n = level t n ++ levelTrees ts n

