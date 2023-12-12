main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

parse :: String -> [(String, [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (s, map read $ words $ map comma num)
    comma c = if c == ',' then ' ' else c

p1 :: [(String, [Int])] -> Int
p1 inp = sum (map count inp)
  where
    count (s, xs) = length $ filter (== xs) $ map runs $ arrangements s
    arrangements :: String -> [String]
    arrangements [] = []
    arrangements "?" = [".", "#"]
    arrangements [c] = [[c]]
    arrangements ('?':cs) = concatMap (\a -> ['.': a, '#':a]) (arrangements cs)
    arrangements (c:cs) = map (\a -> c: a) (arrangements cs)
    runs :: String -> [Int]
    runs [] = []
    runs s = case takeWhile (== '#') s of
        [] -> runs (drop 1 s)
        prefix -> let len = length prefix in len : runs (drop len s)
    matchingRuns :: [Int] -> [[Int]] -> Int
    matchingRuns xs = length . filter (== xs)
