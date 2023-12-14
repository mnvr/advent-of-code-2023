{-# OPTIONS_GHC -Wno-all #-}

import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

parse :: String -> [(String, [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (s, map read $ words $ map comma num)
    comma c = if c == ',' then ' ' else c

-- p1 :: [(String, [Int])] -> Int
p1 = uncurry ways . head

ways :: String -> [Int] -> Int
ways [] [] = 1
ways [] [x] = 0
ways s [] = if none '#' s then 1 else 0
ways ('.':rs) xs = ways rs xs
ways ('?':'.':rs) xs = ways rs xs + ways ('#':'.':rs) xs
ways ('?':'#':rs) xs = ways ('#':'#':rs) xs + ways ('#':rs) xs
ways ('?':'?':rs) xs = ways ('#':'.':rs) xs + ways ('#':'#':rs) xs
ways s (x:rx) | none '.' (take x s) && notAfter x '#' s
  = 1 + ways (drop (x + 1) s) rx
ways _ _ = 0

after :: Int -> Char -> String -> Bool
after x c s = only c (take 1 (drop x s))

notAfter :: Int -> Char -> String -> Bool
notAfter x c s = none c (take 1 (drop x s))

only :: Char -> String -> Bool
only c = all (== c) . nub

none :: Char -> String -> Bool
none c = not . any (== c) . nub
