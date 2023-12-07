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
nums =  catMaybes . nums'
nums' [] = []
nums' s = let (a, b) = break isSpace s in readMaybe a : nums' (dropWhile isSpace b)

p1 :: [Card] -> [Int]
p1 = map matches

matches :: ([Int], [Int]) -> Int
matches (xs, ys) = length $ intersect xs ys

p2 = p1
