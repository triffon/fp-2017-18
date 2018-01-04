module Lists where

import Data.List hiding (fst, snd, enumFromTo, enumFromThenTo,
                       length, (++), reverse, (!!), elem, head, tail, null,
                       init, last, take, drop, splitAt,
                       maximum, minimum, sum, product, and, or, concat,
                       map, filter, foldr, foldl, foldr1, foldl1, scanr, scanl,
                       zip, unzip, zipWith, takeWhile, dropWhile, span, all, any)

import Prelude hiding (fst, snd, enumFromTo, enumFromThenTo,
                       length, (++), reverse, (!!), elem, head, tail, null,
                       init, last, take, drop, splitAt,
                       maximum, minimum, sum, product, and, or, concat,
                       map, filter, foldr, foldl, foldr1, foldl1, scanr, scanl,
                       zip, unzip, zipWith, takeWhile, dropWhile, span, all, any)

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
{-
length [] = 0
length (_:xs) = 1 + length xs
-}

length = foldr (\_ -> (+1)) 0

{-
a ++ b
 | null a    = b
 | otherwise = head a : (tail a ++ b)
-}

{-
[]     ++ l = l
(x:xs) ++ l = x:(xs ++ l)
-}

-- flip f x y = f y x

(++) = flip (foldr (:)) 

{-
reverse l
 | null l    = []
 | otherwise = reverse (tail l) ++ [head l]
-}


{-
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]
-}

--reverse = foldr (\x r -> r ++ [x]) []
--reverse = foldr (\x -> (++[x])) []
reverse = foldl (flip (:)) []

{-
reverse l = iter [] l
  where iter r l
         | null l    = r
         | otherwise = iter ((head l):r) (tail l)
-}

{-reverse = iter []
  where iter r []     = r
        iter r (x:xs) = iter (x:r) xs
-}


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

{-
elem x l = case l of []     -> False
                     (y:ys) -> x == y || elem x ys
-}

elem x = foldr (\y -> (x == y ||)) False

head (x:_) = x
tail (_:t) = t

null [] = True
null _  = False

{-
pythagoreanTriplesFromTo a b = [ (x,y,z) | x <- [a..b], y <- [x+1..b],
                                           z <- [a..b],
                                           x^2 + y^2 == z^2, gcd x y == 1]
-}

pythagoreanTriplesFromTo a b = [ (x,y,z) | x <- [a..b], y <- [x+1..b],
                                           x2 <- [x^2],
                                           y2 <- [y^2],
                                           z <- [round (sqrt (fromIntegral (x2 + y2)))],
                                           x2 + y2 == z^2,
                                           gcd x y == 1 ]

--init l = [ l !! i | i <- [0..length l - 2]]

init [_]    = []
init (x:xs) = x:init xs


-- last l = head (reverse l)
{-
last [x] = x
last (_:xs) = last xs
-}
last l = l !! (length l - 1)

{-
take _ []     = []
take 0 _      = []
take n (x:xs) = x:take (n-1) xs
-}

-- take n l = [ l !! i | i <- [0..(min n (length l))-1]]

{-
drop _ []      = []
drop 0 l       = l
drop n (_:xs)  = drop (n-1) xs
-}

-- drop n l = [ l !! i | i <- [n..length l -1]]

splitAt _ []     = ([],[])
splitAt 0 l      = ([],l )
splitAt n (x:xs) = (x:ts,ds)
  where (ts,ds) = splitAt (n-1) xs

take n l = fst (splitAt n l)
drop n l = snd (splitAt n l)


printList :: Show a => [a] -> String
--printList = foldr1 (flip (.) (' ':) . (++)) . map show
printList = foldr1 (\x y -> x ++ (' ':y)) . map show



maximum [x]    = x
maximum (x:xs) = max x (maximum xs)

minimum [x]    = x
minimum (x:xs) = min x (minimum xs)

{-
sum []     = 0
sum (x:xs) = x + sum xs
-}
sum = foldr (+) 0
{-
product []     = 1
product (x:xs) = x * product xs
-}
product = foldr (*) 1

{-
fact n = product [1..n]

and []     = True
and (x:xs) = x && and xs

or []     = False
or (x:xs) = x || or xs

concat []     = []
concat (x:xs) = x ++ concat xs
-}

and = foldr (&&) True
or  = foldr (||) False
concat = foldr (++) []

-- да се преброят колоните в матрицата, за които има елемент, който е равен на
-- сумата от останалите елементи в матрицата
-- т.е. елементът да е равен на половината от сумата
countCols matrix = length [ col | col <- transpose matrix,
                                  ((sum col) / 2) `elem` col]

{-
map f l = [ f x | x <- l ]
filter p l = [ x | x <- l, p x]
-}

-- pairs l1 l2 = [(x,y) | x <- l1, y <- l2]
pairs l1 l2 = concatMap (\x -> map (\y -> (x,y)) l2) l1
triplets l1 l2 l3 = concatMap (\x -> concatMap (\y -> map (\z -> (x,y,z)) l3) l2) l1

{-
map _ []     = []
map f (x:xs) = f x : map f xs
-}

-- (f . g) x = f (g x)
map f = foldr ((:) . f) []

{-
filter _ []     = []
filter p (x:xs)
 | p x          = x:r
 | otherwise    = r
   where r = filter p xs
-}

--filter p = foldr (\x r -> if p x then x:r else r) []
filter p = foldr (\x -> if p x then (x:) else id) []

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _  [x]    = x
foldr1 op (x:xs) = x `op` (foldr1 op xs)

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 op (x:xs) = foldl op x xs


foldr _  nv []     = nv
foldr op nv (x:xs) = x `op` foldr op nv xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
{-
scanr _  nv []     = [nv]
scanr op nv (x:xs) = x `op` r:rs
  where rs@(r:_) = scanr op nv xs
-}
scanr op nv = foldr (\x rs@(r:_) -> x `op` r:rs) [nv]

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _  nv []     = nv
foldl op nv (x:xs) = foldl op (nv `op` x) xs

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _  nv []     = [nv]
scanl op nv (x:xs) = nv:scanl op (nv `op` x) xs

-- (f . g) x = f (g x) 
scanl2 op nv = reverse . foldl (\rs@(r:_) x -> x `op` r:rs) [nv]

{-
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x,y):zip xs ys
-}

zipWith _ [] _          = []
zipWith _ _ []          = []
zipWith op (x:xs) (y:ys) = x `op` y:zipWith op xs ys

zip = zipWith (,)

-- zipWith --> map^2

unzip :: [(a,b)] -> ([a],[b])
-- unzip l = (map fst l, map snd l)
{-
unzip []          = ([],[])
unzip ((x,y):xys) = (x:xs,y:ys)
  where (xs,ys) = unzip xys
-}

unzip = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([],[])

-- TODO
-- superMap :: ([a] -> b) -> [[a]] -> [b]
-- superMap f ([[x1,x2,x3,...,xn],[y1,y2,y3,...,yn],...,[z1,z2,z3,...,zn]]) -->
--      [f [x1,y1,...,z1], f [x2,y2,...,z2], ...., f [xn,yn,...,zn]]

{-
takeWhile _ [] = []
takeWhile p (x:xs)
 | p x       = x:takeWhile p xs
 | otherwise = []
-}

takeWhile p = foldr (\x r -> if p x then x:r else []) []

dropWhile _ [] = []
dropWhile p (x:xs)
 | p x       = dropWhile p xs
 | otherwise = x:xs

span p l = (takeWhile p l, dropWhile p l)

all p = and . map p
any p = or  . map p
