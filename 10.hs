import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

type Node = (Int, Int)

parse :: String -> (Node, M.Map Node (Node, Node))
parse = ensureStart . neighbours . chunks . lines
  where
    chunks ls = let g = ground ls in zip3 (g : ls) ls (drop 1 ls ++ [g])
    ground (h:_) = length h `replicate` '.'
    enum = zip [0..]
    neighbours ck = foldl f (Nothing, M.empty) (enum ck) where
      f m (y, (p, c, n)) = foldl g m (enum c) where
          g :: (Maybe Node, M.Map Node (Node, Node)) -> (Int, Char) -> (Maybe Node, M.Map Node (Node, Node))
          g r      (x, '.') = r
          g (_, m) (x, 'S') = let k = (x, y) in
            (Just k, case neighboursOfStart (p, c, n) 'S' k of
                (Just n1, Just n2) -> M.insert k (n1, n2) m)
          g (s, m) (x,  i ) = let k = (x, y) in
            case neighbour (p, c, n) i k of
                (Just n1, Just n2) -> (s, M.insert k (n1, n2) m)
                _ -> (s, m)
    neighbour :: (String, String, String) -> Char -> (Int, Int) -> (Maybe (Int, Int), Maybe (Int, Int))
    neighbour (p, c, n) '|' k = (north p k, south n k)
    neighbour (p, c, n) '-' k = (east c k, west c k)
    neighbour (p, c, n) 'L' k = (north p k, east c k)
    neighbour (p, c, n) 'J' k = (north p k, west c k)
    neighbour (p, c, n) '7' k = (south n k, west c k)
    neighbour (p, c, n) 'F' k = (south n k, east c k)
    north p (x, y) = if p !! x `notElem` "|F7" then Nothing else Just (x, y - 1)
    south n (x, y) = if n !! x `notElem` "|LJ" then Nothing else Just (x, y + 1)
    west c (x, y) = if x == 0 || c !! (x-1) `notElem` "-LF" then Nothing
        else Just (x - 1, y)
    east c (x, y) = if x == length c || c !! (x+1) `notElem` "-J7" then Nothing
        else Just (x + 1, y)
    neighboursOfStart :: (String, String, String) -> Char -> (Int, Int) -> (Maybe (Int, Int), Maybe (Int, Int))
    neighboursOfStart (p, c, n) _ k = case catMaybes [
        const (Just k) <$> north p k,
        const (Just k) <$> south n k,
        const (Just k) <$> west c k,
        const (Just k) <$> east c k
        ] of
        [Just a, Just b] -> (Just a, Just b)
    ensureStart (Just s, m) = (s, m)
    ensureStart _ = error "input does not contain a start node"

p1 = id -- maximum . M.elems . dist

dist :: (Node, M.Map Node (Node, Node)) -> (M.Map Node Int)
dist (start, neighbours) = relax (distanceMap neighbours)
  where
    distanceMap = M.update (const (Just 0)) start . M.map (const (maxBound - 1 :: Int))
    relax m = case M.mapAccumWithKey (relaxNode m) False m of
        (True, m') -> relax m'
        (_, m') -> m'
    relaxNode dmap changed key dist = case M.lookup key neighbours of
        Just (n1, n2) -> let d1 = fromJust (M.lookup n1 dmap) + 1
                             d2 = fromJust (M.lookup n2 dmap) + 1
                             d = min d1 d2
                         in if dist > d then (True, d) else (changed, dist)
