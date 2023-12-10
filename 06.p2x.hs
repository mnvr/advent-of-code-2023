import Data.List (find)

main :: IO ()
main = print p2

distance :: Int -> Int -> Int
distance rt t = (rt - t) * t

p2 :: Int
p2 = case (first t d, last' t d) of
    (Just f, Just l) -> l - f + 1
    _ -> 0
  where t = 71530
        d = 940200

first :: Int -> Int -> Maybe Int
first rt d = find (\t -> (distance rt t) > d) [0..rt]

last' :: Int -> Int -> Maybe Int
last' rt d = find (\t -> (distance rt t) > d) [rt,(rt-1)..0]
