{-# OPTIONS_GHC -Wno-all #-}

import Data.Map qualified as M
import Data.List
import Debug.Trace

main :: IO ()
main = interact $ (++ "\n") . show . sum . map p1 . parse

parse :: String -> [([String], [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (words $ map dot s, map read $ words $ map comma num)
    dot c = if c == '.' then ' ' else c
    comma c = if c == ',' then ' ' else c

-- p1 (s, xs) = consume s xs
-- p1 :: (String, b) -> String
p1 (s, xs) = sum $ map (\ss -> consume ss xs) (expand s) -- map variations s -- xs

consume :: [String] -> [Int] -> Int
consume ss xs = consume' ss xs -- let r = trace ("consume " ++ show ss ++ " " ++ show xs ++ " called") consume' ss xs in trace ("consume " ++ show ss ++ " " ++ show xs ++ " will return " ++ show r) r

-- consume ss xs = let r = trace ("consume " ++ show ss ++ " " ++ show xs ++ " called") consume' ss xs in trace ("consume " ++ show ss ++ " " ++ show xs ++ " will return " ++ show r) r

consume' :: [String] -> [Int] -> Int
consume' [] [] = 1
-- consume' [s] [x] = if length s == x then 1 else 0
consume' _ [] = 0
consume' [] _ = 0
consume' (s:ss) (x:xs) = if length s /= x then 0 else consume ss xs

expand :: [String] -> [[String]]
expand [] = [[]]
expand (s:ss)
  | nub s == "#" = map (s:) (expand ss)
  | otherwise = concatMap f (expand ss)
     where f ss = [words v ++ ss | v <- variations s]

variations :: String -> [String]
variations [] = [[]]
variations ('#':s) = map ('#':) (variations s)
variations ('?':s) = concatMap (\s -> ['#':s, ' ':s]) (variations s)
