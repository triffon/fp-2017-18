module Simple where

-- Да се дефинира функция, която приема две числа и връща като резултат
-- дали сумата от делителите на първото число се дели на второто

divisibleBy :: Int -> Int -> Bool
a `divisibleBy` b = a `mod` b == 0

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = noDivisorFrom 2
{-
  where noDivisorFrom i
         | i >= n            = True -- [i;n-1] е празното множество
         | n `divisibleBy` i = False
         | otherwise         = noDivisorFrom (i+1)   -- i < n - 1 и не е делител
-}
  -- noDivisorsFrom i = няма делител на n в интервала [i;n)
  where noDivisorFrom :: Int -> Bool
        noDivisorFrom i = i >= n ||
                          not (n `divisibleBy` i) && noDivisorFrom (i+1)

sumDivisorsDivides :: Int -> Int -> Bool
sumDivisorsDivides n m = sumDivisors `divisibleBy` m
  -- sumDivisorsFrom i = сумата на делителите на n в интервала [i;n]
  where sumDivisors :: Int
        sumDivisors = sumDivisorsFrom 1
        sumDivisorsFrom :: Int -> Int
        sumDivisorsFrom i
         | i > n              = 0      -- [i;n] е празното множество
{-
         | n `divisibleBy` i  = i + rest
         | otherwise          = rest
-}
         | otherwise          = maybeI + rest
          where rest = sumDivisorsFrom (i+1)
                maybeI = if n `divisibleBy` i then i else 0

derive :: Double -> (Double -> Double) -> Double -> Double
derive dx f x = (f (x + dx) - f x) / dx

myDerive = derive 0.0001

repeated :: (a -> a) -> Int -> a -> a
repeated _ 0 x = x
repeated f n x = repeated f (n-1) (f x)

{-
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))
-}

-- accumulate :: (t -> t -> t) -> t -> Int -> Int -> (Int -> t) -> (Int -> Int) -> t
accumulate op nv a b term next
 | a > b       = nv
 | otherwise   = term a `op` accumulate op nv (next a) b term next

fact n = accumulate (*) 1 1 n id (1+)

isPrime2 n = accumulate op True 2 (n-1) id (1+)
  where -- d `op` primeSoFar = primeSoFar && not (n `divisibleBy` d)
        op d = (&& (not (n `divisibleBy` d)))
        -- да "добавим" d означава да "добавим" конюкт "d не дели n"

isPrime3 n = not (accumulate (||) False 2 (n-1) (divisibleBy n) (1+))
  -- искаме за никео число от 2 до n-1 да не е делител на х
