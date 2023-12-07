import Data.Bifunctor (bimap, second)
import Data.List (intersect)

main :: IO ()
main = interact $ (++ "\n") . show . ((,) <$> p1 <*> p2) . parseCards

type Card = ([Int], [Int])

parseCards :: String -> [Card]
parseCards s = map (bimap nums nums . span (/= "|") . tail . words) (lines s)
  where nums = map read . tail

p1 :: [Card] -> Int
p1 = sum . map points

matches :: Card -> Int
matches (xs, ys) = length $ intersect xs ys

points :: Card -> Int
points card = case matches card of 0 -> 0; n -> 2 ^ (n - 1)

p2 :: [Card] -> Int
p2 = sum . map (+1) . foldr f [] where
  f card wins = w : wins where
    p = matches card
    w = p + sum (take p wins)
