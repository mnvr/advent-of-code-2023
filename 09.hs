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

p1 xs = if az xs then [xs] else let d = dxs xs in xs : p1 d
  where az = all (==0)

dxs :: [Int] -> [Int]
dxs xs = zipWith (-) (drop 1 $ xs) xs
