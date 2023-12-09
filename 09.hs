{-# OPTIONS_GHC -Wno-all #-}

import Data.Char (isSpace)
import Data.Bifunctor (bimap)

main :: IO ()
main = interact $ (++ "\n") . show . p2a . parse

parse :: String -> [[Int]]
parse = map nums . lines

nums :: String -> [Int]
nums [] = []
nums s = uncurry (:) <$> bimap read nums $ break isSpace (dropWhile isSpace s)

p1 :: [[Int]] -> Int
p1 = sum . map (foldr (\ds d -> last ds + d) 0 . dxs)

p2a :: [[Int]] -> Int
p2a = map (foldl (\d ds -> head ds - d) 0 . reverse . dxs)

-- p2 :: [[Int]] -> Int
-- p2 xss = sum $ map p2' xss
p2 xss = map p2' xss

-- p2' :: [Int] -> Int
-- p2' :: [Int] -> [[Int]]
p2' xs = fst $ foldl (\(d, yss) ds -> (head ds - d, yss ++ [ds])) (0,[]) $ dxs xs
--foldl (\(d, yss) ds -> (d + head ds, (d + head ds : yss))) (0, []) $ dxs xs

dxs :: [Int] -> [[Int]]
dxs xs = if all (==0) xs then [xs] else xs : dxs (dx xs)

dx :: [Int] -> [Int]
dx xs = zipWith (-) (drop 1 $ xs) xs
