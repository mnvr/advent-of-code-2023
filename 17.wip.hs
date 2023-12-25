import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Sequence (Seq(..), fromList, (><))
import Data.List (find)

main :: IO ()
main = interact $ unlines . p1 . parse

type Node = (Int, Int)
data Grid a = Grid { items :: M.Map Node a, lastNode :: Node } deriving Show

data Direction = L | R | U | D deriving (Show, Eq, Ord)
data Cell = Cell {
  node :: Node, direction :: Direction,
  -- The number of blocks that we have already moved in this direction.
  moves :: Int }
  deriving  (Show, Eq, Ord)

data Neighbour = Neighbour { cell :: Cell, distance :: Int }

parse :: String -> Grid Int
parse s = Grid { items = M.fromList xs, lastNode = fst (last xs) }
  where xs = [((x, y), read [c]) | (y, l) <- enum (lines s), (x, c) <- enum l]

enum :: [a] -> [(Int, a)]
enum = zip [0..]

visitor :: (Show a) => Cell -> a -> String
visitor cell item = "visiting item " ++ show item ++ " at " ++ show cell

neighbours :: Grid Int -> Cell -> [Neighbour]
neighbours Grid { items } = mapMaybe toNeighbour . adjacent
  where
    toNeighbour :: Cell -> Maybe Neighbour
    toNeighbour cell = case M.lookup (node cell) items of
      Just d | moves cell < 4 -> Just (Neighbour cell d)
      _ -> Nothing
    adjacent :: Cell -> [Cell]
    adjacent Cell { node = (x, y), direction, moves } = case direction of
      L -> [Cell (x + 1, y) L (moves + 1), Cell (x, y - 1) U 1, Cell (x, y + 1) D 1]
      R -> [Cell (x - 1, y) R (moves + 1), Cell (x, y - 1) U 1, Cell (x, y + 1) D 1]
      U -> [Cell (x, y - 1) U (moves + 1), Cell (x - 1, y) R 1, Cell (x + 1, y) L 1]
      D -> [Cell (x, y + 1) D (moves + 1), Cell (x - 1, y) R 1, Cell (x + 1, y) L 1]

-- Find the shortest path from start to an end using Dijkstra's algorithm.
dijkstra :: Grid Int -> Node -> (Cell -> Bool) -> (Cell -> Int -> String) -> (Maybe Int, [String])
dijkstra grid@Grid { items } start isEnd visitor =
  go (M.singleton startCell 0) M.empty S.empty
  where
    -- Since moves is 0, the direction doesn't distinguish between moving ahead
    -- or turning.
    startCell = Cell { node = start, direction = D, moves = 0 }
    visit x = let item = fromJust $ M.lookup (node x) items in visitor x item
    next ds seen = (M.lookupMin $ M.withoutKeys ds seen)
    go ds parent seen = case next ds seen of
        Nothing -> (Nothing, [])
        Just (u, du)
          | isEnd u -> (Just du, [visit u] ++ showDistanceMap grid ds parent u)
          | otherwise ->
             let (ds', parent') = foldl (relax u du) (ds, parent) (neighbours grid u)
                 (d', vs) = go ds' parent' (S.insert u seen)
             in (d', visit u : vs)

    relax :: Cell -> Int -> (M.Map Cell Int, M.Map Cell Cell)
             -> Neighbour -> (M.Map Cell Int, M.Map Cell Cell)
    relax u du (ds, parent) Neighbour { cell = v, distance = d } =
      case M.lookup v ds of
        Just dv | dv < du + d -> (ds, parent)
        _ -> (M.insert v (du + d) ds, M.insert v u parent)

showDistanceMap :: Grid a -> M.Map Cell Int -> M.Map Cell Cell -> Cell -> [String]
showDistanceMap Grid { lastNode = (mx, my) } ds parent end = map line [0..my]
  where
    path = retrace S.empty end
      where retrace s n = let s' = S.insert n s in case M.lookup n parent of
              Nothing -> s'
              Just p -> retrace s' p
    isOnPath cell = S.member cell path
    line y = unwords $ map dist [0..mx]
      where dist x = showCell $ find isOnPath [
              Cell {node = (x, y), direction = d, moves }
              | d <- [L, R, U, D], moves <- [0..3]]
            showCell Nothing = "   .   "
            showCell (Just cell@Cell { node, moves }) =
              " " ++ d ++ " " ++ show moves ++ " "
                where d = pad3 $ show $ fromJust $ M.lookup cell ds
                      pad3 s = reverse $ take 3 (reverse ("   " ++ s))


p1 :: Grid Int -> [String]
p1 grid = let (r, zs) = dijkstra grid (0, 0) isEnd visitor
          in zs ++ ["shortest-path result " ++ (show $ fromMaybe (-1) r)]
  where
    isEnd Cell { node } = node == (lastNode grid)
