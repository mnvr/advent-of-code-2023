import Data.List

main :: IO ()
main = interact $ (++ "\n") . show . parse

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
        adjustY = maybe 0 id (findIndex (> y) missingY)
        adjustX = maybe 0 id (findIndex (> x) missingX)
