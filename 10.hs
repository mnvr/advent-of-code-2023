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
          g r      (x, '.') = r
          g (_, m) (x, 'S') = let k = (x, y) in
            (Just k, M.insert k (neighboursOfStart p c n y x) m)
          g (s, m) (x,  i ) = let k = (x, y) in (s, M.insert k (neighbour i k) m)
    neighbour '|' p = (north p, south p)
    neighbour '-' p = (east p, west p)
    neighbour 'L' p = (north p, east p)
    neighbour 'J' p = (north p, west p)
    neighbour '7' p = (south p, west p)
    neighbour 'F' p = (south p, east p)
    north = second (subtract 1)
    south = second (+ 1)
    west = first (subtract 1)
    east = first (+ 1)
    neighboursOfStart p c n y x = case nos' p c n y x of
        [a,b] -> (a, b)
        [a] -> (a, (x+1,y))
    nos' p c n y x = catMaybes [
        if p !! x `elem` "|F7" then Just (x,y-1) else Nothing,
        if n !! x `elem` "|LJ" then Just (x,y+1) else Nothing,
        if x > 0 && c !! (x-1) `elem` "-LF" then Just (x-1, y) else Nothing]
    ensureStart (Just s, m) = (s, m)
    ensureStart _ = error "input does not contain a start node"

p1 = maximum . M.elems . dist

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
