-- WIP!
-- A simpler, barebones version of the solution with a foldr.

import Data.Bifunctor
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.List (intersect)

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseCards

type Card = ([Int], [Int])

parseCards :: String -> [Card]
parseCards s = map (bimap nums nums . span (/= '|')) (lines s)

nums :: String -> [Int]
nums = catMaybes . nums' where
  nums' [] = []
  nums' s = let (a, b) = break isSpace s in readMaybe a : nums' (dropWhile isSpace b)

p1 :: [Card] -> Int
p1 = sum . map points

matches :: Card -> Int
matches (xs, ys) = length $ intersect xs ys

points :: Card -> Int
points card = case matches card of 0 -> 0; n -> 2 ^ (n - 1)

p2 = foldr f [] where
  f card wins = let p = matches card in p : wins
