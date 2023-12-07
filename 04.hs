import Data.Bifunctor (bimap, second)
import Data.List (intersect)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parseCards

type Card = ([Int], [Int])

parseCards :: String -> [Card]
parseCards = map (bimap nums nums . span (/= "|") . tail . words) . lines
  where nums = map read . tail

p1 :: [Card] -> Int
p1 = sum . map points

matches :: Card -> Int
matches = length . uncurry intersect

points :: Card -> Int
points card = case matches card of 0 -> 0; n -> 2 ^ (n - 1)

p2 :: [Card] -> Int
p2 = sum . map (+1) . foldr f [] where
  f card wins = w : wins where
    m = matches card
    w = m + sum (take m wins)
