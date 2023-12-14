import Control.Arrow ((&&&))
import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . lines
-- main = interact $ (++ "\n") . p1 . lines

north, south, east, west, cycle1, cycle1B :: [String] -> [String]
north = transpose . map f . transpose
west = map f
south = transpose . map reverse . map f . map reverse . transpose
east = map reverse . map f . map reverse
cycle1 = east . south . west . north
cycle1B = (\(h:_) -> h) . drop 3 . iterate cycle1
-- cycle1B = (\(h:_) -> h) . drop 1000000000 . iterate cycle1

f :: String -> String
f s = let (a, b) = span (/= '#') s
      in (reverse $ sort a) ++ (
        let (c, d) = break (/= '#') b in c ++ (
            if null d then [] else f d))

load :: [String] -> Int
load = sum . map g . countdown
 where
    countdown xs = let n = length xs in zip [n, n-1 ..] xs
    g (i, s) = i * (length $ filter (== 'O') s)

p1, p2 :: [String] -> Int
p1 = load . north
p2 = load . cycle1B

p2v = unlines . cycle1B
