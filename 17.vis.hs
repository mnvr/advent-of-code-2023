import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust, fromMaybe)
import Data.List (find)

-- A variant of 17.hs that also prints the route after finding it.

main :: IO ()
main = interact $ unlines . (\grid -> concat [p1 grid, p2 grid]) . parse

type Node = (Int, Int)
data Grid a = Grid { items :: M.Map Node a, lastNode :: Node }

data Direction = L | R | U | D deriving (Eq, Ord)
data Cell = Cell {
  node :: Node, direction :: Direction,
  -- The number of blocks that we have already moved in this direction.
  moves :: Int }
  deriving  (Eq, Ord)

data Neighbour = Neighbour { cell :: Cell, distance :: Int }

parse :: String -> Grid Int
parse s = Grid { items = M.fromList xs, lastNode = fst (last xs) }
  where xs = [((x, y), read [c]) | (y, l) <- enum (lines s), (x, c) <- enum l]

enum :: [a] -> [(Int, a)]
enum = zip [0..]

neighbours :: Grid Int -> [Int] -> Cell -> [Neighbour]
neighbours Grid { items } range = filter inRange . concat . adjacent
  where
    adjacent Cell { node = (x, y), direction, moves } = case direction of
      L -> [cells (\m -> Cell (x + m, y) L (moves + m)),
            cells (\m -> Cell (x, y - m) U m),
            cells (\m -> Cell (x, y + m) D m)]
      R -> [cells (\m -> Cell (x - m, y) R (moves + m)),
            cells (\m -> Cell (x, y - m) U m),
            cells (\m -> Cell (x, y + m) D m)]
      U -> [cells (\m -> Cell (x, y - m) U (moves + m)),
            cells (\m -> Cell (x - m, y) R m),
            cells (\m -> Cell (x + m, y) L m)]
      D -> [cells (\m -> Cell (x, y + m) D (moves + m)),
            cells (\m -> Cell (x - m, y) R m),
            cells (\m -> Cell (x + m, y) L m)]
    cells c = snd (foldl (\(d, xs) m -> toNeighbour (c m) d xs) (0, []) extent)
    extent = [1..maximum range]
    toNeighbour cell d xs = case M.lookup (node cell) items of
      Just d2 -> (d + d2, Neighbour cell (d + d2) : xs)
      _ -> (d, xs)
    inRange Neighbour { cell } = moves cell `elem` range

-- Find the shortest path from start to an end using Dijkstra's algorithm.
dijkstra :: Grid Int -> Node -> (Cell -> Bool) -> [Int] -> (Maybe Int, [String])
dijkstra grid@Grid { items } start isEnd range =
  go (M.singleton startCell 0) M.empty S.empty (singleton (0, startCell))
  where
    -- By setting moves to 0, the starting cell's considers both the left and
    -- down neighbours as equivalent (which is what we want).
    startCell = Cell { node = start, direction = L, moves = 0 }

    go ds parent seen q = case extractMin q of
        Nothing -> (Nothing, [])
        Just ((du, u), q')
          | isEnd u -> (Just du, showDistanceMap grid ds parent u range)
          | u `S.member` seen -> go ds parent seen q'
          | otherwise ->
             let adj = neighbours grid range u
                 (ds', parent', q'') = foldl (relax u du) (ds, parent, q') adj
             in go ds' parent' (S.insert u seen) q''

    relax u du (ds, parent, q) Neighbour { cell = v, distance = d } =
      let d' = du + d in case M.lookup v ds of
        Just dv | dv < d' -> (ds, parent, q)
        _ -> (M.insert v d' ds, M.insert v u parent, insert (d', v) q)

data Heap a = Empty | Heap a (Heap a) (Heap a)

union :: Ord a => Heap a -> Heap a -> Heap a
union Empty h = h
union h Empty = h
union hl@(Heap l ll lr) hr@(Heap r _ _)
  | l <= r = Heap l (union hr lr) ll
  | otherwise = union hr hl

extractMin :: Ord a => Heap a -> Maybe (a, Heap a)
extractMin Empty = Nothing
extractMin (Heap x l r) = Just (x, union l r)

singleton :: a -> Heap a
singleton x = Heap x Empty Empty

insert :: Ord a => a -> Heap a -> Heap a
insert x h = singleton x `union` h

showDistanceMap :: Grid a -> M.Map Cell Int -> M.Map Cell Cell -> Cell -> [Int] -> [String]
showDistanceMap Grid { lastNode = (mx, my) } ds parent end range = map line [0..my]
  where
    path = retrace S.empty end
      where retrace s n = let s' = S.insert n s in case M.lookup n parent of
              Nothing -> s'
              Just p -> retrace s' p
    isOnPath cell = S.member cell path
    line y = unwords $ map dist [0..mx]
      where dist x = showCell $ find isOnPath [
              Cell {node = (x, y), direction = d, moves }
              | d <- [L, R, U, D], moves <- range]
            showCell Nothing = "   .   "
            showCell (Just cell@Cell { node, moves }) =
              " " ++ d ++ " " ++ show moves ++ " "
                where d = pad3 $ show $ fromJust $ M.lookup cell ds
                      pad3 s = reverse $ take 3 (reverse ("   " ++ s))

p1, p2 :: Grid Int -> [String]
p1 grid = runP grid [1..3]
p2 grid = runP grid [4..10]

runP :: Grid Int -> [Int] -> [String]
runP grid range = let (r, zs) = dijkstra grid (0, 0) isEnd range
          in zs ++ ["shortest-path result " ++ (show $ fromMaybe (-1) r)]
  where
    isEnd Cell { node } = node == (lastNode grid)
