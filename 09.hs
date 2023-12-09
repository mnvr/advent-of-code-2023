{-# OPTIONS_GHC -Wno-all #-}

import Data.Char (isSpace)
import Data.Bifunctor (bimap)

main :: IO ()
main = interact $ (++ "\n") . show . p2 . parse

nums :: String -> [Int]
nums [] = []
nums s = uncurry (:) <$> bimap read nums $ break isSpace (dropWhile isSpace s)

parse :: String -> [[Int]]
parse = map nums . lines

-- p1 :: [Int] -> [[Int]]
p1 xss = sum $ map p1' xss

p1' xs = last . head $ fst $ foldr (\ds (yss, d) -> ((ds ++ [last ds + d]) : yss, last ds + d)) ([], 0) $ dxs xs

-- p2 :: [[Int]] -> Int
p2 xss = sum $ map p2' xss

-- p2' :: [Int] -> Int
-- p2' :: [Int] -> [[Int]]
p2' xs = fst $ foldl (\(d, yss) ds -> (head ds - d, yss ++ [ds])) (0,[]) $ dxs xs
--foldl (\(d, yss) ds -> (d + head ds, (d + head ds : yss))) (0, []) $ dxs xs

dxs xs = if az xs then [xs] else let d = dx xs in xs : dxs d
  where az = all (==0)

dx :: [Int] -> [Int]
dx xs = zipWith (-) (drop 1 $ xs) xs
