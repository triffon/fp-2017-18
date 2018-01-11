module Lazy where

import Prelude hiding (repeat, cycle, iterate)

-- ones = 1 : ones
nats = [0..]
-- ones = [1,1..]
ones = repeat 1

--repeat x = [x,x..]
repeat x = x : repeat x

cycle :: [a] -> [a]
cycle l = l ++ cycle l
-- !!! cycle l = cycle l ++ l

iterate :: (a -> a) -> a -> [a]
iterate f z = z:iterate f (f z)

pairs = [ (x,y) | x <- [1..], y <- [1..x] ]

pythagoreanTriples = [ (x, y, z) | z <- [1..], x <- [1..z], y <- [1..x],
                                   x^2 + y^2 == z^2, gcd x y == 1 ]

triplets = iterate (map (+3)) [3,2,1]

concatTriplets = foldr (++) [] triplets

-- sublist n m l = take m (drop n l)

-- sublist n m = take m . drop n

-- flip f x y = f y x
sublist = (.) (flip take) . flip drop

-- on f g x y = f (g x) (g y)

-- comp f g x y = f (g x y)

-- comp f g = (f .) . g

f x = f $! (1 - x)


fakeseq x y = y

-- ($!) = ap seq
