import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence (Seq(..), fromList, (><))

main :: IO ()
main = interact $ unlines . dsp . parse
  where
    dsp grid = let (r, zs) = dijkstra grid (0, 0) (lastNode grid) visitor
               in zs ++ ["shortest-path result " ++ (show $ fromMaybe (-1) r)]

type Node = (Int, Int)
data Grid a = Grid { items :: M.Map Node a, lastNode :: Node } deriving Show

parse :: String -> Grid Int
parse s = Grid { items = M.fromList xs, lastNode = fst (last xs) }
  where xs = [((x, y), read [c]) | (y, l) <- enum (lines s), (x, c) <- enum l]

enum :: [a] -> [(Int, a)]
enum = zip [0..]

visitor :: (Show a) => (Int, Int) -> a -> String
visitor node item = "visiting item " ++ show item ++ " at " ++ show node

neighbours :: Grid a -> Node -> [Node]
neighbours Grid { items } = filter (`M.member` items) . adjacent
  where adjacent (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

distance :: Grid Int -> Node -> Node -> Int
distance Grid { items } u v = fromJust $ M.lookup v items

-- Find the shortest path from start, to end, using Dijkstra's algorithm.
dijkstra :: Grid Int -> Node -> Node -> (Node -> Int -> b) -> (Maybe Int, [b])
dijkstra grid@Grid { items } start end visitor = go (M.singleton start 0) S.empty
  where
    visit x = let item = fromJust $ M.lookup x items in visitor x item
    next ds seen = (M.lookupMin $ M.withoutKeys ds seen)
    go ds seen = case next ds seen of
        Nothing -> (Nothing, [])
        Just (u, du)
          | u == end -> (M.lookup u ds, [visit u])
          | otherwise ->
             let ds' = foldl (relax u du) ds (neighbours grid u)
                 (d', vs) = go ds' (S.insert u seen)
             in (d', visit u : vs)

    relax :: Node -> Int -> M.Map Node Int -> Node -> M.Map Node Int
    relax u du ds v = let d = distance grid u v in case M.lookup v ds of
                         Just dv | dv < du + d -> ds
                         _ -> M.insert v (du + d) ds

