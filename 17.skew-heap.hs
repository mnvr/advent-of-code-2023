import Data.Map qualified as M
import Data.Set qualified as S
import Control.Arrow ((&&&))

-- A variant of 17.hs that implements and uses a skew heap for the priority
-- queue. This is a slower than using the Data.Set (that 17.hs uses), e.g. 17.hs
-- takes ~10s optimized, whilst this takes ~14s. Theoretically though, this skew
-- heap has the same O(log n) amortized behaviour as the standard set.

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Node = (Int, Int)
data Grid a = Grid { items :: M.Map Node a, lastNode :: Node }

parse :: String -> Grid Int
parse s = Grid { items = M.fromList xs, lastNode = fst (last xs) }
  where xs = [((x, y), read [c]) | (y, l) <- enum (lines s), (x, c) <- enum l]

enum :: [a] -> [(Int, a)]
enum = zip [0..]

data Direction = L | R | U | D deriving (Eq, Ord)
data Cell = Cell {
  node :: Node, direction :: Direction,
  -- The number of blocks that we have already moved in this direction.
  moves :: Int }
  deriving  (Eq, Ord)

data Neighbour = Neighbour { cell :: Cell, distance :: Int }

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

shortestPath :: [Int] -> Grid Int -> Int
shortestPath moveRange grid@Grid { items, lastNode } =
  go (M.singleton startCell 0) S.empty (singleton (0, startCell))
  where
    -- By setting moves to 0, the starting cell's considers both the left and
    -- down neighbours as equivalent (which is what we want).
    startCell = Cell { node = (0, 0), direction = L, moves = 0 }
    isEnd Cell { node } = node == lastNode

    go ds seen q = case extractMin q of
      Nothing -> 0
      Just ((du, u), q')
        | isEnd u -> du
        | S.member u seen -> go ds seen q'
        | otherwise -> let adj = neighbours grid moveRange u
                           (ds', q'') = foldl (relax u du) (ds, q') adj
                       in go ds' (S.insert u seen) q''

    relax u du (ds, q) Neighbour { cell = v, distance = d } =
      let d' = du + d in case M.lookup v ds of
        Just dv | dv < d' -> (ds, q)
        _ -> (M.insert v d' ds, insert (d', v) q)

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

p1, p2 :: Grid Int -> Int
p1 = shortestPath [1..3]
p2 = shortestPath [4..10]
