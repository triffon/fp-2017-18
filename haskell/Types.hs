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

-- katniss = Player("Katniss Everdeen", 45")

better :: Player -> Player -> Player
better p1@(Player {score = score1}) p2@(Player {score = score2})
  | score1 > score2 = p1
  | otherwise       = p2
