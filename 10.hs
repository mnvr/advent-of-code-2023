import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (catMaybes, fromJust)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Node = (Int, Int)

data Parsed = Parsed {
  start :: Node,
  neighbours :: M.Map Node [Node],
  vertices :: S.Set Node
  }

parse :: String -> Parsed
parse = mkParsed . chunks . lines
  where
    mkParsed ck = let (s, nb) = ensureStart (neighbours ck)
      in Parsed { start = s, neighbours = nb, vertices = verts ck }

    chunks ls = let g = ground ls in zip3 (g : ls) ls (drop 1 ls ++ [g])
    ground (h:_) = length h `replicate` '.'

    verts ck = foldl f S.empty (enum ck) where
      f vs (y, (_, l, _)) = foldl g vs (enum l) where
          g vs (x, ch) | ch `elem` "SFLJ7" = S.insert (y, x) vs
                      | otherwise = vs

    neighbours ck = foldl f (Nothing, M.empty) (enum ck) where
      f m (y, ck@(_, c, _)) = foldl g m (enum c) where
          g r (_, '.') = r
          g (_, m) (x, 'S') = let k = (y, x) in
            (Just k, M.insert k (neighboursOfStart ck 'S' k) m)
          g (s, m) (x,  i ) =
            let k = (y, x)
                nb = (neighbour ck i k)
            in if null nb then (s, m) else (s, M.insert k nb m)

    neighbour ck '|' k = catMaybes [north ck k, south ck k]
    neighbour ck '-' k = catMaybes [west ck k, east ck k]
    neighbour ck 'L' k = catMaybes [north ck k, east ck k]
    neighbour ck 'J' k = catMaybes [north ck k, west ck k]
    neighbour ck '7' k = catMaybes [south ck k, west ck k]
    neighbour ck 'F' k = catMaybes [south ck k, east ck k]

    north (p, _, _) (y, x)
      | p !! x `elem` "|F7S" = Just (y - 1, x)
      | otherwise = Nothing
    south (_, _, n) (y, x)
      | n !! x `elem` "|LJS" = Just (y + 1, x)
      | otherwise = Nothing
    west (_, c, _) (y, x)
      | x > 0 && c !! (x - 1) `elem` "-LFS" = Just (y, x - 1)
      | otherwise = Nothing
    east (_, c, _) (y, x)
      | x + 1 < length c && c !! (x + 1) `elem` "-J7S" = Just (y, x + 1)
      | otherwise = Nothing

    neighboursOfStart ck _ key = catMaybes [
        north ck key, south ck key, west ck key, east ck key]

    ensureStart (Just s, m) = (s, m)
    ensureStart _ = error "input does not contain a start node"

enum :: [a] -> [(Int, a)]
enum = zip [0..]

-- We're guaranteed to have a path looping back to itself from the start, just
-- follow it, no need for any state. The maximum distance will then be exactly
-- half of this (since this is a cartesian grid).
maxDist :: Node -> M.Map Node [Node] -> Int
maxDist s n = (loopLength s n) `div` 2

loopLength :: Node -> M.Map Node [Node] -> Int
loopLength start neighbours = go 0 start start
  where
    go c n prev | n == start && prev /= start = c
    go c n prev = case M.lookup n neighbours of
      Just ns -> let (next:_) = dropWhile (== prev) ns in go (c+1) next n

-- The Shoelace formula is an easy to understand (if you see it visually) way to
-- compute the area of a cartesian polygon. The gist is that we see the polygon
-- as a composed of trapezoids defined by each consecutive pair of nodes
-- (ignoring colinear edges). Some of these pairs will have a positive
-- contribution, others will have a negative contribution. So if we sum them up,
-- we'll get the area (or its negation, depending on the direction we go, thus
-- we take the absolute value to ignore that issue).

shoelaceArea :: Node -> M.Map Node [Node] -> S.Set Node -> Int
shoelaceArea start neighbours vertices = go 0 start start start
  where
    go a n since prev | n == start && prev /= start = let z = (abs (a + shoelace since n)) `div` 2 in z
    go a n since prev = case M.lookup n neighbours of
      Just ns ->  let (next:_) = filter (/= prev) ns in
        if S.member n vertices then go (a + shoelace since n) next n n else go a next since n
    shoelace (y, x) (y', x') = let z = area1 (y, x) (y', x') in z
    area1 (y, x) (y', x') = (y + y') * (x - x')


-- Pick's formula gives us the way to relate the area of a polygon with integer
-- coordinates for all its vertices in terms of the number of integer points
-- within and on it.
--
-- Let i be the number of integer points interior to the polygon. This is what
-- we wish to find.
--
-- Let a be the area of the polygon. This we can find using the shoelace
-- function above.
--
-- Let b be the number of integer points on boundary. This is the pathLength.
--
-- Then, the area of this polygon is
--
-- A = i + (b/2)  - 1
--
-- Or, for us, the number of interior points is
--
-- i = A - (b/2) + 1
interiorPoints i b = i - (b `div` 2) + 1

p1 :: Parsed -> Int
p1 Parsed { start, neighbours } = maxDist start neighbours

-- p2 :: Parsed -> Int
p2 Parsed { start, neighbours, vertices } =
   let len = loopLength start neighbours
       a = shoelaceArea start neighbours vertices
       i = interiorPoints a len
   in i
