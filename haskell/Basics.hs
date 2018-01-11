module Basics where

-- x :: Int
x = 2
-- x = 1
-- y :: Double
y = x^^2 + 7.5
-- z :: String
z = "hello"

-- square :: Int -> Int
square x = x * x

div50 = div 50
twice f x = f (f x)
diag f x = f x x
hypothenuse a b = sqrt (a**2 + b**2)
g a b c = a + b + c

fact x
 | n == 0 = 1
 | n > 0  = n * fact (n - 1)
 | n < 0  = error "подадено отрицателно число"
 where n = x

{-
True  && x = x
False && _ = False
-}

-- reverse([X|XS], R) :- ... 

x `h` y = x + y

f x y = y
