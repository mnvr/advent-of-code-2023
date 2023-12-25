import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust, fromMaybe, mapMaybe, listToMaybe, isJust)
import Data.List (find, minimumBy)

main :: IO ()
main = interact $ unlines . (\grid -> concat [p1 grid, p2 grid]) . parse

type Node = (Int, Int)
data Grid a = Grid { items :: M.Map Node a, lastNode :: Node } deriving Show

data Direction = L | R | U | D deriving (Show, Eq, Ord)
data Cell = Cell {
  node :: Node, direction :: Direction,
  -- The number of blocks that we have already moved in this direction.
  moves :: Int }
  deriving  (Show, Eq, Ord)

data Neighbour = Neighbour { cell :: Cell, distance :: Int } deriving Show

parse :: String -> Grid Int
parse s = Grid { items = M.fromList xs, lastNode = fst (last xs) }
  where xs = [((x, y), read [c]) | (y, l) <- enum (lines s), (x, c) <- enum l]

enum :: [a] -> [(Int, a)]
enum = zip [0..]

neighbours :: Grid Int -> [Int] -> Cell -> [Neighbour]
neighbours Grid { items } range = adjacent
  where
    toNeighbour :: Cell -> Int -> [Neighbour] -> (Int, [Neighbour])
    toNeighbour cell d xs = case M.lookup (node cell) items of
      Just d2 | moves cell `elem` range -> (d + d2, Neighbour cell (d + d2) : xs)
      Just d2 -> (d + d2, xs)
      _ -> (d, xs)
    adjacent :: Cell -> [Neighbour]
    adjacent Cell { node = (x, y), direction, moves } =
      let rng = [1..maximum range] in case direction of
        L -> concat [
          snd (foldl (\(d, xs) m -> let cell = Cell (x + m, y) L (moves + m)
                                    in toNeighbour cell d xs) (0, []) rng),
          snd (foldl (\(d, xs) m -> let cell = Cell (x, y - m) U m
                                    in toNeighbour cell d xs) (0, []) rng),
          snd (foldl (\(d, xs) m -> let cell = Cell (x, y + m) D m
                                    in toNeighbour cell d xs) (0, []) rng)
          ]

        R -> concat [
          snd (foldl (\(d, xs) m -> let cell = Cell (x - m, y) R (moves + m)
                                    in toNeighbour cell d xs) (0, []) rng),
          snd (foldl (\(d, xs) m -> let cell = Cell (x, y - m) U m
                                    in toNeighbour cell d xs) (0, []) rng),
          snd (foldl (\(d, xs) m -> let cell = Cell (x, y + m) D m
                                    in toNeighbour cell d xs) (0, []) rng)
          ]

        U -> concat [
          snd (foldl (\(d, xs) m -> let cell = Cell (x, y - m) U (moves + m)
                                    in toNeighbour cell d xs) (0, []) rng),
          snd (foldl (\(d, xs) m -> let cell = Cell (x - m, y) R m
                                    in toNeighbour cell d xs) (0, []) rng),
          snd (foldl (\(d, xs) m -> let cell = Cell (x + m, y) L m
                                    in toNeighbour cell d xs) (0, []) rng)
          ]

        D -> concat [
          snd (foldl (\(d, xs) m -> let cell = Cell (x, y + m) D (moves + m)
                                    in toNeighbour cell d xs) (0, []) rng),
          snd (foldl (\(d, xs) m -> let cell = Cell (x - m, y) R m
                                    in toNeighbour cell d xs) (0, []) rng),
          snd (foldl (\(d, xs) m -> let cell = Cell (x + m, y) L m
                                    in toNeighbour cell d xs) (0, []) rng)
          ]

-- Find the shortest path from start to an end using Dijkstra's algorithm.
dijkstra :: Grid Int -> Node -> (Cell -> Bool)
            -> [Int]
            -> (Maybe Int, [String])
dijkstra grid@Grid { items } start isEnd range =
  go (M.singleton startCell 0) M.empty S.empty (h_singleton (0, startCell))
  where
    -- Since moves is 0, the direction doesn't distinguish between moving ahead
    -- or turning.
    startCell = Cell { node = start, direction = L, moves = 0 }
    go :: M.Map Cell Int -> M.Map Cell Cell -> S.Set Cell ->  Heap (Int, Cell) -> (Maybe Int, [String])
    go ds parent seen q = case extractMin q of
        Nothing -> case nearestEnd ds of
                     Nothing -> (Nothing, [])
                     Just (u, du) -> (Just du, showDistanceMap grid ds parent u range)
        Just ((du, u), q')
          | u `S.member` seen -> go ds parent seen q'
          | otherwise ->
             let adj = (neighbours grid range u)
                 adj' = filter (\Neighbour {cell} -> cell `S.notMember` seen) adj
                 (ds', parent', q'') = foldl (relax u du) (ds, parent, q') adj
                 (d', vs) = go ds' parent' (S.insert u seen) q''
             in (d', vs)

    relax :: Cell -> Int -> (M.Map Cell Int, M.Map Cell Cell, Heap (Int, Cell))
             -> Neighbour -> (M.Map Cell Int, M.Map Cell Cell, Heap (Int, Cell))
    relax u du (ds, parent, q) Neighbour { cell = v, distance = d } =
      case M.lookup v ds of
        Just dv | dv < du + d -> (ds, parent, q)
        _ -> (M.insert v (du + d) ds, M.insert v u parent, h_insert (du + d, v) q)

    nearestEnd ds = case map (\k -> (k, fromJust (M.lookup k ds))) $ filter (\k -> isEnd k) (M.keys ds) of
      [] -> Nothing
      kvs -> Just $ minimumBy (\(k, v) (k2, v2) -> v `compare` v2) kvs


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

h_singleton :: a -> Heap a
h_singleton x = Heap x Empty Empty

h_insert :: Ord a => a -> Heap a -> Heap a
h_insert x h = h_singleton x `union` h

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
