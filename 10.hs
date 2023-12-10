import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Control.Arrow ((&&&))

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
          g (_, m) (x, 'S') = let k = (y, x) in
            (Just k, case neighboursOfStart (p, c, n) 'S' k of
                (Just n1, Just n2) -> M.insert k (n1, n2) m)
          g (s, m) (x,  i ) = let k = (y, x) in
            case neighbour (p, c, n) i k of
                (Just n1, Just n2) -> (s, M.insert k (n1, n2) m)
                -- z -> error (show ((y, x), i, z))
                _ -> (s, m)
    neighbour :: (String, String, String) -> Char -> (Int, Int) -> (Maybe (Int, Int), Maybe (Int, Int))
    neighbour (p, c, n) '|' k = (north p k, south n k)
    neighbour (p, c, n) '-' k = (west c k, east c k)
    neighbour (p, c, n) 'L' k = (north p k, east c k)
    neighbour (p, c, n) 'J' k = (north p k, west c k)
    neighbour (p, c, n) '7' k = (south n k, west c k)
    neighbour (p, c, n) 'F' k = (south n k, east c k)
    north p (y, x) = if p !! x `notElem` "|F7S" then Nothing else Just (y - 1, x)
    south n (y, x) = if n !! x `notElem` "|LJS" then Nothing else Just (y + 1, x)
    west c (y, x) = if x == 0 || c !! (x - 1) `notElem` "-LFS" then Nothing
        else Just (y, x - 1)
    east c (y, x) = if x + 1 == length c || c !! (x + 1) `notElem` "-J7S" then Nothing
        else Just (y, x + 1)
    neighboursOfStart :: (String, String, String) -> Char -> (Int, Int) -> (Maybe (Int, Int), Maybe (Int, Int))
    neighboursOfStart (p, c, n) _ (y, x) = case catMaybes [
        if p !! x `elem` "|F7" then Just (y - 1, x) else Nothing,
        if n !! x `elem` "|LJ" then Just (y + 1, x) else Nothing,
        if x > 0 && c !! (x - 1) `elem` "-LF" then Just (y, x - 1) else Nothing,
        if x + 1 < length c && c !! (x + 1) `elem` "-J7" then Just (y, x + 1) else Nothing
        ] of
        [a, b] -> (Just a, Just b)
    ensureStart (Just s, m) = (s, m)
    ensureStart _ = error "input does not contain a start node"

p1 = dist --maximum . M.elems . dist
-- p1 = dist

-- dist :: (Node, M.Map Node (Node, Node)) -> (M.Map Node Int)
-- dist (start, neighbours) = pruneUnreachable $ relax' (distanceMap neighbours)
dist (start, neighbours) = relax (distanceMap start) []
  where
    inf = (maxBound - 1 :: Int)
    -- distanceMap = M.update (const (Just 0)) start . M.map (const inf)
    distanceMap s = M.singleton start 0
    -- initialPending = [0]
    relax dm [] = dm

    -- relax dmap pending = case foldl f dmap pending of
    --     [] -> r
    --  where f m key = case (M.lookup key m, M.lookup key neighbours) of
    --           (Just dist, Just (n1, n2)) -> case (M.lookup n1 dmap,  M.lookup n2 dmap) of
    --             (Just d1, Just d2) -> updateIfLower (min d1 d2)
    --             (Just d1, _) -> updateIfLower d1
    --             (_, Just d2) -> updateIfLower d2
    --             _ -> (changed, dist)
    --          where updateIfLower d = if (d + 1) < dist then (True, d + 1) else (changed, dist)

    -- pruneUnreachable = M.filter (/= inf)
    -- relax' m = case M.mapAccumWithKey (relaxNode' m) False m of
    --     (True, m') -> relax' m'
    --     (_, m') -> m'
    -- relaxNode' dmap changed key dist = case M.lookup key neighbours of
    --     Just (n1, n2) -> case (M.lookup n1 dmap,  M.lookup n2 dmap) of
    --         (Just d1, Just d2) -> updateIfLower (min d1 d2)
    --         (Just d1, _) -> updateIfLower d1
    --         (_, Just d2) -> updateIfLower d2
    --         _ -> (changed, dist)
    --     where updateIfLower d = if (d + 1) < dist then (True, d + 1) else (changed, dist)
