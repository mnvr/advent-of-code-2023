{-# OPTIONS_GHC -Wno-all #-}

import Data.List (nub)
import Debug.Trace (trace)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

parse :: String -> [(String, [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (s, map read $ words $ map comma num)
    comma c = if c == ',' then ' ' else c

-- p1 :: [(String, [Int])] -> Int
p1 = sum . map (uncurry ways)

ways :: String -> [Int] -> Int
ways = ways' ways
ways_ = f
  where f s xs = let z = ways' f s xs in trace ("ways " ++ show s ++ " " ++ show xs ++ " " ++ show z) z

ways' :: Num a => ([Char] -> [Int] -> a) -> [Char] -> [Int] -> a
ways' f [] [] = 1
ways' f [] [x] = 0
ways' f s [] = if none '#' s then 1 else 0
ways' f ('.':rs) xs = f rs xs
ways' f ('?':rs) xs = f rs xs + f ('#':rs) xs
ways' f s (x:rx) | length s >= x && none '.' (take x s) && notAfter x '#' s
  = f (drop (x + 1) s) rx
ways' _ _ _ = 0

after :: Int -> Char -> String -> Bool
after x c s = only c (take 1 (drop x s))

notAfter :: Int -> Char -> String -> Bool
notAfter x c s = none c (take 1 (drop x s))

only :: Char -> String -> Bool
only c = all (== c) . nub

none :: Char -> String -> Bool
none c = not . any (== c) . nub
