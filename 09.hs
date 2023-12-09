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

p1 = id
