import Data.List
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Galaxy = (Int, Int)

parse :: String -> [Galaxy]
parse = concatMap (uncurry row) . enum . lines
  where
    row y = foldl item [] . enum
      where item gs (x, '#') = (y, x) : gs
            item gs _ = gs

enum :: [a] -> [(Int, a)]
enum = zip [0..]

expand :: Int -> [Galaxy] -> [Galaxy]
expand by gs = map f gs
  where
    ys = nub (map fst gs)
    xs = nub (map snd gs)
    missingY = [0 .. maximum ys] \\ ys
    missingX = [0 .. maximum xs] \\ xs
    m = by - 1
    f (y, x) = (y + m * adjustY, x + m * adjustX)
      where
        adjustY = maybe (length missingY) id (findIndex (> y) missingY)
        adjustX = maybe (length missingX) id (findIndex (> x) missingX)

pairs :: [a] -> [(a, a)]
pairs gs = concat $ zipWith f [0..] gs where f i u = map (u,) (drop (i + 1) gs)

dist :: Galaxy -> Galaxy -> Int
dist (y, x) (y', x') = abs (y - y') + abs (x - x')

p1 :: [Galaxy] -> Int
p1 = sumD . expand 2

p2 :: [Galaxy] -> Int
p2 = sumD . expand 1000000

sumD :: [Galaxy] -> Int
sumD = sum . map (uncurry dist) . pairs
