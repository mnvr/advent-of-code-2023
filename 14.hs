import Control.Arrow ((&&&))
import Data.List
import Debug.Trace

main :: IO ()
-- main = interact $ (++ "\n") . show . (p1 &&& p2) . lines
main = interact $ (++ "\n") . show . p2s . lines
-- main = interact $ (++ "\n") . p1 . lines

north, south, east, west, cycle1, cycle1B :: [String] -> [String]
north = transpose . map f . transpose
west = map f
south = transpose . map reverse . map f . map reverse . transpose
east = map reverse . map f . map reverse
cycle1 = east . south . west . north
cycle1B = (\(h:_) -> h) . drop 3 . iterate cycle1
-- cycle1B = (\(h:_) -> h) . drop 1000000000 . iterate cycle1

cycleUntilStable :: Int -> [String] -> [String]
cycleUntilStable c xs = trace ("iteration " ++ show c ++ "\n" ++ unlines xs) $ let ys = cycle1 xs in if xs == ys then ys else cycleUntilStable (c + 1) ys

cycleUntilStable2 c prev xs = trace ("iteration " ++ show c ++ " previous size " ++ show (length prev) ++ "\n" ++ unlines xs) $
  let ys = cycle1 xs in case findIndex (==ys) prev of
    Nothing -> cycleUntilStable2 (c + 1) (prev ++ [ys]) ys
    Just i -> (i, length prev - i)

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
-- p2s = cycleUntilStable 0
p2s = cycleUntilStable2 0 []
-- p2b = load . (\(h:_) -> h) . drop 30000 . iterate cycle1
