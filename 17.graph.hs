-- An exploration of basic graph algorithms using the example from day 17 as an
-- example graph.

import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . dfs . parse

type Node = (Int, Int)
data Grid = Grid { items :: M.Map Node Int, maxNode :: Node } deriving Show

parse :: String -> Grid
parse s = Grid { items = M.fromList xs, maxNode = fst (last xs) }
  where xs = [((x, y), read [c]) | (y, l) <- enum (lines s), (x, c) <- enum l]

enum :: [a] -> [(Int, a)]
enum = zip [0..]

dfs = id
