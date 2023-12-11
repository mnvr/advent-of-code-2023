import Data.List ((\\), nub)

main :: IO ()
main = interact $ (++ "\n") . show . parse

type Galaxy = (Int, Int)

parse = concatMap (uncurry row) . enum . lines
  where
    row y = foldl item []. enum
      where item gs (x, '#') = (y, x) : gs
            item gs _ = gs

expandX :: [Galaxy] -> [Galaxy]
expandX gs = gs
  where
    xs = nub (map snd gs)
    maxX = maximum xs
    missing = [0..maxX] \\ xs

enum :: [a] -> [(Int, a)]
enum = zip [0..]
