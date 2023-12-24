-- An exploration of basic graph algorithms using the example from day 17 as an
-- example graph.

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust)
import Data.Sequence (Seq(..), fromList, (><))

main :: IO ()
main = interact $ unlines . demo . parse
  where
    demo grid = concat [ddfs grid, dbfs grid]
    ddfs grid = dfs grid (0, 0) (visitor "dfs")
    dbfs grid = bfs grid (0, 0) (visitor "bfs")

type Node = (Int, Int)
data Grid a = Grid { items :: M.Map Node a, maxNode :: Node } deriving Show

parse :: String -> Grid Int
parse s = Grid { items = M.fromList xs, maxNode = fst (last xs) }
  where xs = [((x, y), read [c]) | (y, l) <- enum (lines s), (x, c) <- enum l]

enum :: [a] -> [(Int, a)]
enum = zip [0..]

visitor :: (Show a) => String -> (Int, Int) -> a -> String
visitor label node item =
    label ++ " visiting item " ++ show item ++ " at " ++ show node

neighbours :: Grid a -> Node -> [Node]
neighbours Grid { items } = filter (`M.member` items) . adjacent
  where adjacent (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

dfs :: Grid a -> Node -> (Node -> a -> b) -> [b]
dfs grid@Grid { items } start visitor = go [start] S.empty
  where
    visit x = let item = fromJust $ M.lookup x items in visitor x item
    go [] seen = []
    go (x:xs) seen | S.member x seen = go xs seen
    go (x:xs) seen = visit x : go ((neighbours grid x) ++ xs) (S.insert x seen)

-- Data.Sequence provides us with an efficient queue.
bfs :: Grid a -> Node -> (Node -> a -> b) -> [b]
bfs grid@Grid { items } start visitor = go (Empty :|> start) S.empty
  where
    visit x = let item = fromJust $ M.lookup x items in visitor x item
    go Empty seen = []
    go (xs :|> x) seen | S.member x seen = go xs seen
    go (xs :|> x) seen = visit x : go ys (S.insert x seen)
      where ys = (fromList $ neighbours grid x) >< xs
