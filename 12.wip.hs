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

-- p1 (s, xs) = consume s xs
p1 (s, xs) = expand s -- xs

-- consume :: [String] -> [Int] -> Int
-- consume ss xs = let r = trace ("consume " ++ show ss ++ " " ++ show xs ++ " called") consume' ss xs in trace ("consume " ++ show ss ++ " " ++ show xs ++ " will return " ++ show r) r

-- consume' :: [String] -> [Int] -> Int
-- consume' [] [] = 1
-- consume' [s] [x] | nub s == "#" = if length s == x then 1 else 0
-- consume' [s] [x] | otherwise = consume1 s [] [x]
-- consume' _ [] = 0
-- consume' [] _ = 0
-- consume' (s:ss) (x:xs) | nub s == "#" =
--     -- s is a group consisting solely of #, and thus must be entirely consumed
--     if length s /= x then 0 else consume ss xs
-- consume' (s:ss) (x:xs) | nub s == "?" =
--     -- s is a group consisting solely of ?, so we can
--     let a = consume ss (x:xs)    -- skip it entirely
--         b = consume1 s ss (x:xs) -- or consume it entirely or partially
--     in a + b
-- consume' (s:ss) (x:xs) =
--     -- s is a group consisting of a mix of # and ?, so we must consume it at
--     -- least partially (if not entirely).
--     consume1 s ss xs

expand :: [String] -> [[String]]
expand [] = [[]]
expand (s:ss)
  | nub s == "#" = map (s:) (expand ss)
  | nub s == "?" = concatMap (\ss -> [s:ss, ss]) (expand ss)
  | otherwise = concatMap f (expand ss)
     where f ss = [words v ++ ss | v <- variations s]

variations :: String -> [String]
variations [] = [[]]
variations ('#':s) = map ('#':) (variations s)
variations ('?':s) = concatMap (\s -> ['#':s, ' ':s]) (variations s)

matchEx :: String -> Int -> Bool
matchEx s x = length s == x
