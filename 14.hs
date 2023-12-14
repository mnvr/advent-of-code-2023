import Control.Arrow ((&&&))
import Data.List
import Debug.Trace

main :: IO ()
main = interact $ (++ "\n") . show . p1 . lines

north :: [String] -> [String]
north = transpose . map f . transpose
 where
    f s = let (a, b) = span (/= '#') s in (reverse $ sort a) ++ (let (c, d) = break (/= '#') b in c ++ (if null d then [] else f d))

p1 :: [String] -> Int
p1 = sum . map g . countdown . north
 where
    countdown xs = let n = length xs in zip [n,n-1..] xs
    g (i, s) = i * (length $ filter (== 'O') s)
