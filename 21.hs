import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

type Index = (Int, Int)
type Garden = M.Map Index Char

parse :: String -> Garden
parse = foldl row M.empty . enum . lines
  where row m (y, line) = foldl cell m (enum line)
          where cell m (x, c) = M.insert (x, y) c m

enum :: [a] -> [(Int, a)]
enum = zip [0..]

p1 :: Garden -> Garden
p1 = id
