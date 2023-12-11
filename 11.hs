import Data.List
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Galaxy = (Int, Int)

parse :: String -> [Galaxy]
parse = concatMap (uncurry row) . enum . lines
  where
    row y = foldl item []. enum
      where item gs (x, '#') = (y, x) : gs
            item gs _ = gs

enum :: [a] -> [(Int, a)]
enum = zip [0..]

expand :: Int -> [Galaxy] -> [Galaxy]
expand by gs = sort $ map f gs
  where
    ys = nub (map fst gs)
    xs = nub (map snd gs)
    missingY = sort $ [0..maximum ys] \\ ys
    missingX = sort $ [0..maximum xs] \\ xs
    f (y, x) = (y + (by * adjustY), x + (by * adjustX))
      where
        adjustY = maybe (length missingY) id (findIndex (> y) missingY)
        adjustX = maybe (length missingX) id (findIndex (> x) missingX)

pairs :: [a] -> [(a, a)]
pairs gs = concatMap f [0..length gs - 1]
  where f i = let u = gs !! i in map (\v -> (u, v)) (drop (i + 1) gs)

dist :: Galaxy -> Galaxy -> Int
dist (y, x) (y', x') = abs (y - y') + abs (x - x')

p1 :: [Galaxy] -> Int
p1 = sumD . expand 1

p2 :: [Galaxy] -> Int
p2 = sumD . expand 10

sumD :: [Galaxy] -> Int
sumD = sum . map (uncurry dist) . pairs
