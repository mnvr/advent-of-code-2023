{-# OPTIONS_GHC -Wno-all #-}

import Data.Char (isSpace)
import Data.Bifunctor (bimap)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

parse :: String -> [[Int]]
parse = map nums . lines

nums :: String -> [Int]
nums [] = []
nums s = uncurry (:) <$> bimap read nums $ break isSpace (dropWhile isSpace s)

p1 :: [[Int]] -> Int
p1 = sum . map (foldr (\ds d -> last ds + d) 0 . dxs)

p2 :: [[Int]] -> Int
p2 = sum . map (foldl (\d ds -> head ds - d) 0 . reverse . dxs)

dxs :: [Int] -> [[Int]]
dxs xs = if all (==0) xs then [xs] else xs : dxs (dx xs)

dx :: [Int] -> [Int]
dx xs = zipWith (-) (drop 1 $ xs) xs
