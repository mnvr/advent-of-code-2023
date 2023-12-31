-- An exploration of basic graph algorithms using the example from day 17 as an
-- example graph.

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (Seq(..), fromList, (><))

main :: IO ()
main = interact $ unlines . demo . parse
  where
    demo grid = concatMap ($ grid) [ddfs, dbfs, dsp]
    ddfs grid = dfs grid (0, 0) (visitor "dfs")
    dbfs grid = bfs grid (0, 0) (visitor "bfs")
    dsp grid = let end = maxNode grid
                   (r, zs) = dijkstra grid (0, 0) end (visitor "shortest-path")
               in zs ++ ["shortest-path result " ++ (show $ fromMaybe (-1) r)]

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

-- For our example, the weight of the edge between u and v is the value
-- at v. But this can be any arbitrary function or input data.
distance :: Grid Int -> Node -> Node -> Int
distance Grid { items } u v = fromJust $ M.lookup v items

-- Find the shortest path from start, to end, using Dijkstra's algorithm.
dijkstra :: Grid Int -> Node -> Node -> (Node -> Int -> b) -> (Maybe Int, [b])
dijkstra grid@Grid { items } start end visitor = go (M.singleton start 0) S.empty
  where
    visit x = let item = fromJust $ M.lookup x items in visitor x item
    next ds seen = M.foldrWithKey f Nothing $ M.withoutKeys ds seen
      where f u du (Just (v, dv)) | dv < du = Just (v, dv)
            f u du _ = Just (u, du)
    go ds seen = case next ds seen of
        Nothing -> (Nothing, [])
        Just (u, du)
          | u == end -> (M.lookup u ds, [visit u])
          | otherwise ->
             let v = visit u
                 seen' = S.insert u seen
                 ds' = foldl (relax u du) ds (neighbours grid u)
                 (d', vs) = go ds' seen'
             in (d', v : vs)

    relax :: Node -> Int -> M.Map Node Int -> Node -> M.Map Node Int
    relax u du ds v = let d = distance grid u v in case M.lookup v ds of
                         Just dv | dv < du + d -> ds
                         _ -> M.insert v (du + d) ds

