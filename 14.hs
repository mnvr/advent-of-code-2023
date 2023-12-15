import Control.Arrow ((&&&))
import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . lines

north, south, east, west, cycle1, cycle1B :: [String] -> [String]
north = transpose . map f . transpose
west = map f
south = transpose . map reverse . map f . map reverse . transpose
east = map reverse . map f . map reverse

f :: String -> String
f [] = []
f s = let (a, b) = span (/= '#') s
          (c, d) = break (/= '#') b
      in reverse (sort a) ++ c ++ f d

cycle1 = east . south . west . north
cycle1B xs = go 1000000000 [(load xs, xs)] xs
  where
    go n prev xs =
      let key = (load &&& id) $ cycle1 xs in case findIndex (==key) prev of
          Nothing -> go n (key : prev) (snd key)
          Just i ->
            let cycleLength = i + 1
                offset =  length prev - (i + 1)
                remain = (n - offset) `mod` cycleLength
            in (\(h:_)-> snd h) $ drop remain $ drop offset $ reverse prev

load :: [String] -> Int
load = sum . map g . countdown
 where
    countdown xs = let n = length xs in zip [n, n-1 ..] xs
    g (i, s) = i * (length $ filter (== 'O') s)

p1, p2 :: [String] -> Int
p1 = load . north
p2 = load . cycle1B
