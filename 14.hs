import Control.Arrow ((&&&))
import Data.List
import Debug.Trace

main :: IO ()
main = interact $ (++ "\n") . p2 . lines

north :: [String] -> [String]
north = transpose . map f . transpose

f s = let (a, b) = span (/= '#') s in (reverse $ sort a) ++ (let (c, d) = break (/= '#') b in c ++ (if null d then [] else f d))

west = map f
south = transpose . map reverse . map f . map reverse . transpose
east = map reverse . map f . map reverse
-- sounwest = map f

cycle1 = east . south . west . north

load = sum . map g . countdown
 where
    countdown xs = let n = length xs in zip [n,n-1..] xs
    g (i, s) = i * (length $ filter (== 'O') s)

p1 :: [String] -> Int
p1 = load . north
 where
    countdown xs = let n = length xs in zip [n,n-1..] xs
    g (i, s) = i * (length $ filter (== 'O') s)

-- p2 = load . cycle10k
p2 = unlines . cycle10k

cycle10k = (\(h:_) -> h) . drop 3 . iterate cycle1
-- cycle10k = (\(h:_) -> h) . drop 1000000000 . iterate cycle1
