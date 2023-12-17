import Data.Char (digitToInt)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

data Grid = Grid { gv :: [[Int]], mx, my :: Int }

parse :: String -> Grid
parse = mkG . map (map digitToInt) . lines
  where mkG xs@(h:_) = Grid xs (length h) (length xs)

p1 = (`path` (0, 0))

path :: Grid -> (Int, Int) -> Int
path Grid { gv, mx, my } start = go start 0 3
  where
    go :: (Int, Int) -> Int -> Int -> Int
    go (x, y) c _ | (x, y) == (mx, my) = c
    go (x, y) c t = 0
