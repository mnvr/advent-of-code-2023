{-# OPTIONS_GHC -Wno-all #-}

import Data.Char (isSpace)
import Data.Bifunctor (bimap)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

nums :: String -> [Int]
nums [] = []
nums s = uncurry (:) <$> bimap read nums $ break isSpace (dropWhile isSpace s)

parse :: String -> [Int]
parse = nums . head . lines

p1 :: [Int] -> [[Int]]
p1 xs = fst $ foldr (\ds (yss, d) -> ((ds ++ [last ds + d]) : yss, last ds + d)) ([], 0) $ dxs xs

dxs xs = if az xs then [xs] else let d = dx xs in xs : dxs d
  where az = all (==0)

dx :: [Int] -> [Int]
dx xs = zipWith (-) (drop 1 $ xs) xs
