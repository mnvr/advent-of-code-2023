{-# OPTIONS_GHC -Wno-all #-}

import Data.Char (isSpace)
import Data.Bifunctor (bimap)

main :: IO ()
main = interact $ (++ "\n") . show . parse

readInt :: String -> Int
readInt = read
-- nums :: String -> ([Int], String)
nums [] = []
nums s = let (a, b) = bimap readInt nums $ break isSpace (dropWhile isSpace s) in a:b

-- parse :: String -> [Int]
parse = nums . head . lines
