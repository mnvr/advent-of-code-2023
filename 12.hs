{-# OPTIONS_GHC -Wno-all #-}

import Data.Map qualified as M
import Data.List
import Debug.Trace

main :: IO ()
main = interact $ (++ "\n") . show . p1 . head . parse

parse :: String -> [(String, [Int])]
parse = map line . lines
  where
    line l = case words l of
        [s, num] -> (s, map read $ words $ map comma num)
    comma c = if c == ',' then ' ' else c

p1 (s, xs) = consume s xs

consume :: String -> [Int] -> Int
consume s [] = trace ("base case " ++ s) $ if dropWhile (== '.') s == "" then 1 else 0
consume s (x:xs) = trace ("normal case for x " ++ show x ++ "  " ++ s) $
    if take (x + 1) s == (x `replicate` '#') ++ "." then trace ("case a") $ consume (drop (x + 1) s) xs
    else if take (x + 1) s == (x `replicate` '#') ++ "?" then trace ("case b") $ consume (drop (x + 1) s) xs
    else if take (x + 1) s == (x `replicate` '#') ++ [] then trace ("case c") $ consume (drop (x + 1) s) xs
    else if take 1 s == "." then trace ("case d") $ consume (dropWhile (== '.') s) (x:xs)
    else if take 1 s == "?" then trace ("case e") $ (
        let a = consume (drop 1 s) (if x == 1 then xs else (x-1):xs) -- taking ? as #
            b = consume (drop 1 s) (x:xs)                            -- taking ? as .
        in trace ("case e " ++ show (x:xs) ++ " " ++ s ++ "  got a " ++ show a ++ " b " ++ show b) $ if a == 0 then b else if b == 0 then a else a + b)
    else 0

