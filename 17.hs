import Data.Map qualified as M
import Data.Set qualified as S
import Control.Arrow ((&&&))

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
data Cell = Cell { node :: Node, direction :: Direction } deriving  (Eq, Ord)
data Neighbour = Neighbour { cell :: Cell, distance :: Int }

neighbours :: Grid Int -> [Int] -> Cell -> [Neighbour]
neighbours Grid { items } range = concat . adjacent
  where
    adjacent Cell { node = (x, y), direction = d }
      | d `elem` [L, R] = [cells (\m -> Cell (x, y - m) U),
                           cells (\m -> Cell (x, y + m) D)]
      | otherwise = [cells (\m -> Cell (x - m, y) R),
                     cells (\m -> Cell (x + m, y) L)]
    cells mkCell = snd $ foldl (mkNeighbour mkCell) (0, []) [1..maximum range]
    mkNeighbour mkCell (d, xs) m = let cell = mkCell m in case M.lookup (node cell) items of
      Just d2 -> (d + d2, if m `elem` range then Neighbour cell (d + d2) : xs else xs)
      _ -> (d, xs)

shortestPath :: [Int] -> Grid Int -> Int
shortestPath moveRange grid@Grid { lastNode } = go startDist S.empty startQ
  where
    -- Start in both directions so that we never have to go straight and can
    -- just always turn. This way, we don't even need to track moves.
    startCells = [Cell { node = (0, 0), direction = L },
                  Cell { node = (0, 0), direction = D }]
    startDist = M.fromList $ zip startCells [0, 0]
    startQ = S.fromList $ zip [0, 0] startCells
    isEnd Cell { node } = node == lastNode

    go ds seen q = case S.minView q of
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
        _ -> (M.insert v d' ds, S.insert (d', v) q)

p1, p2 :: Grid Int -> Int
p1 = shortestPath [1..3]
p2 = shortestPath [4..10]
