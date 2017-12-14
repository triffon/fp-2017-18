module Lists where

import Prelude hiding (fst, snd, enumFromTo, enumFromThenTo,
                       length, (++), reverse, (!!), elem, head, tail, null)

fst (x,_) = x
snd (_,y) = y

type Student = (String, Int, Double)

skipsProblemExam (_, 44444, _)  = True
skipsProblemExam ("Иван", _, _) = False
skipsProblemExam (_, fn, grade) = grade >= 5 && fn >= 40000

(x,y) = (3.5,7.8)

-- На Пролог: [1|[2|[3|[4|[]]]]]

{-
enumFromTo :: Int -> Int -> [Int]
enumFromTo from to
  | from > to     = []
  | otherwise     = from:enumFromTo (from+1) to
-}
enumFromTo from to = enumFromThenTo from (from+1) to

enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo from next to
  | from > to     = []
  | otherwise     = from:enumFromThenTo next (next+dx) to
    where dx = next - from
-- next == from + dx
-- dx == next - from
-- next <- next + dx == next + next - from

length :: [a] -> Int

{-
length l
 | null l    = 0
 | otherwise = 1 + length (tail l)
-}
length [] = 0
length (_:xs) = 1 + length xs

{-
a ++ b
 | null a    = b
 | otherwise = head a : (tail a ++ b)
-}

[]     ++ l = l
(x:xs) ++ l = x:(xs ++ l)

{-
reverse l
 | null l    = []
 | otherwise = reverse (tail l) ++ [head l]
-}

{-
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]
-}

{-
reverse l = iter [] l
  where iter r l
         | null l    = r
         | otherwise = iter ((head l):r) (tail l)
-}

reverse = iter []
  where iter r []     = r
        iter r (x:xs) = iter (x:r) xs

{-
l !! 0 = head l
l !! n = tail l !! (n - 1)
-}

[]     !! _ = error "Не може да вземем елемент от празен списък"
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n-1)

{-
elem x l
  | null l      = False
  | x == head l = True
  | otherwise   = elem x (tail l)
-}

{-
elem x l = not (null l) && (x == head l || elem x (tail l))
-}

{-
elem _ []     = False
elem x (y:ys) = x == y || elem x ys
-}

elem x l = case l of []     -> False
                     (y:ys) -> x == y || elem x ys

head (x:_) = x
tail (_:t) = t

null [] = True
null _  = False
