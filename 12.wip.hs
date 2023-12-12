{-# OPTIONS_GHC -Wno-all #-}

import Data.List (intercalate)

main :: IO ()
main = interact $ (++ "\n") . p1 . parse

parse :: String -> [([String], [Int])]
parse = map line . lines
  where
    line l = case unfold (words l) of
        [s, num] -> (words $ map dot s, map read $ words $ map comma num)
    dot c = if c == '.' then ' ' else c
    comma c = if c == ',' then ' ' else c

-- Set this to 1 to toggle between p1 and p2
rc = 1 :: Int

unfold :: [String] -> [String]
unfold [s, xs] = [intercalate "?" (replicate rc s), intercalate "," (replicate rc xs)]

p1' :: [([String], [Int])] -> Int
p1' = sum . map count
  where count (ss, xs) = sum $ map (`consume` xs) (expand ss)

p1 :: [([String], [Int])] -> [Char]
p1 zz = unlines $ map f zz
  where
    f (s, xs) = g s xs
    g (a:b) (x:xs)
      | take 1 a == "#" && a == x `replicate` '#' = g b xs
      | not (null b) && not (null xs) && take 1 (last b) == "#" && (last b) == (last xs) `replicate` '#' = g (a:unlast b) (x:unlast xs)
      | length a == x = g b xs
      | not (null b) && not (null xs) && length (last b) == last xs = g (a:unlast b) (x:unlast xs)
      | length a > x && '#' `elem` (take x a) = g ((drop (x + 2) a):b) xs
    g [] [] = show 1
    g [s] [x] | '#' `notElem` s = show $ length s `nCr` x
    g u v = "reduced to " ++ show u ++ "  " ++ show v

unlast = reverse . drop 1 . reverse

nCr n r = fact n `div` fact (n - r)
fact n = product [1..n]

consume :: [String] -> [Int] -> Int
consume [] [] = 1
consume _  [] = 0
consume []  _ = 0
consume (s:ss) (x:xs) = if length s /= x then 0 else consume ss xs

expand :: [String] -> [[String]]
expand [] = [[]]
expand (s:ss)
  | '?' `notElem` s = map (s:) (expand ss)
  | otherwise = concatMap f (expand ss)
     where f ss = [words v ++ ss | v <- variations s]

variations :: String -> [String]
variations [] = [[]]
variations ('#':s) = map ('#':) (variations s)
variations ('?':s) = concatMap (\s -> ['#':s, ' ':s]) (variations s)
