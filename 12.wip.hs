{-# OPTIONS_GHC -Wno-all #-}

import Data.Map qualified as M
import Data.List
import Debug.Trace

main :: IO ()
main = interact $ (++ "\n") . show . p1 . head . parse

parse :: String -> [([String], [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (words $ map dot s, map read $ words $ map comma num)
    dot c = if c == '.' then ' ' else c
    comma c = if c == ',' then ' ' else c

p1 (s, xs) = consume s xs

consume :: [String] -> [Int] -> Int
consume ss xs = let r = trace ("consume " ++ show ss ++ " " ++ show xs ++ " called") consume' ss xs in trace ("consume " ++ show ss ++ " " ++ show xs ++ " will return " ++ show r) r

consume' :: [String] -> [Int] -> Int
consume' [] [] = 1
consume' [s] [x] = if matchEx s x then 1 else 0
consume' _ [] = 0
consume' [] _ = 0
consume' (s:ss) (x:xs) =
    let a = consume ss (x:xs)
        b = consume (s:ss) xs
        c = consume ss xs
    in if matchEx s x then a + b + c else 0

matchEx :: String -> Int -> Bool
matchEx s x = length s == x

-- match :: String -> Int -> Int
-- match s x =
--     if length s == x then 1
--     else if length s < x then 0
--     else

