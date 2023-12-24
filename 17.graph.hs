-- An exploration of basic graph algorithms using the example from day 17 as an
-- example graph.

import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . demo . parse
  where demo grid = dfs grid (0, 0) visitor

type Node = (Int, Int)
data Grid a = Grid { items :: M.Map Node a, maxNode :: Node } deriving Show

parse :: String -> Grid Int
parse s = Grid { items = M.fromList xs, maxNode = fst (last xs) }
  where xs = [((x, y), read [c]) | (y, l) <- enum (lines s), (x, c) <- enum l]

enum :: [a] -> [(Int, a)]
enum = zip [0..]

visitor :: (Show a) => (Int, Int) -> a -> String
visitor node item = "visiting item " ++ show item ++ " at " ++ show node

dfs :: Grid a -> Node -> (Node -> a -> b) -> [b]
dfs Grid { items, maxNode = (mx, my) } start f = []
