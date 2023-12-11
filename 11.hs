import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

type Galaxy = (Int, Int)

parse :: String -> [Galaxy]
parse = expand . concatMap (uncurry row) . enum . lines
  where
    row y = foldl item []. enum
      where item gs (x, '#') = (y, x) : gs
            item gs _ = gs

enum :: [a] -> [(Int, a)]
enum = zip [0..]

expand :: [Galaxy] -> [Galaxy]
expand gs = sort $ map f gs
  where
    ys = nub (map fst gs)
    xs = nub (map snd gs)
    missingY = sort [0..maximum ys] \\ ys
    missingX = [0..maximum xs] \\ xs
    f (y, x) = (y + adjustY, x + adjustX)
      where
        adjustY = maybe (length missingY) id (findIndex (> y) missingY)
        adjustX = maybe (length missingX) id (findIndex (> x) missingX)

pairs :: [a] -> [(a, a)]
pairs gs = concatMap f [0..length gs - 1]
  where f i = let u = gs !! i in map (\v -> (u, v)) (drop (i + 1) gs)


dist :: Galaxy -> Galaxy -> Int
dist (y, x) (y', x') = abs (y - y') + abs (x - x')


p1 :: [Galaxy] -> Int
p1 = sum . map (uncurry dist) . pairs
