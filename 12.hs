{-# OPTIONS_GHC -Wno-all #-}

main :: IO ()
main = interact $ (++ "\n") . show . p1 . head . parse

parse :: String -> [(String, [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (s, map read $ words $ map comma num)
    comma c = if c == ',' then ' ' else c

p1 = go
  where
    go (s, xs) = let as = arrangements s in map (\a -> (a, satisfies xs a, runs a, xs == (runs a))) as
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
    satisfies ::  [Int] -> String -> Bool
    satisfies [] _ = True
    satisfies _ [] = False
    satisfies xs ('.':rs) = satisfies xs rs
    satisfies xs@(x:r) s =
        if take x s == x `replicate` '#' then satisfies r (drop x s) else False
