import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (catMaybes, fromJust)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Node = (Int, Int)

data Parsed = Parsed { start :: Node
                     , vertices :: S.Set Node
                     , neighbours :: M.Map Node [Node] }

parse :: String -> Parsed
parse = mkParsed . chunks . lines
  where
    mkParsed ck = let (s, nb) = ensureStart (neighbours ck)
      in Parsed { start = s, neighbours = nb, vertices = verts ck }

    chunks ls = let g = ground ls in zip3 (g : ls) ls (drop 1 ls ++ [g])
    ground (h:_) = length h `replicate` '.'
    enum = zip [0..]
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

    north (p, _, _) (y, x) = if p !! x `elem` "|F7S"
      then Just (y - 1, x) else Nothing
    south (_, _, n) (y, x) = if n !! x `elem` "|LJS"
      then Just (y + 1, x) else Nothing
    west (_, c, _) (y, x) = if x > 0 && c !! (x - 1) `elem` "-LFS"
      then Just (y, x - 1) else Nothing
    east (_, c, _) (y, x) = if x + 1 < length c && c !! (x + 1) `elem` "-J7S"
      then Just (y, x + 1) else Nothing

    neighboursOfStart ck _ key = catMaybes [
        north ck key, south ck key, west ck key, east ck key]
    ensureStart (Just s, m) = (s, m)
    ensureStart _ = error "input does not contain a start node"

-- We're guaranteed to have a path looping back to itself from the start, and we
-- can just follow it linearly to count the length on the nodes on it.
loopLength :: Parsed -> Int
loopLength Parsed { start, neighbours } = go 0 start start
  where
    go c n prev | n == start && prev /= start = c
    go c n prev = case M.lookup n neighbours of
      Just ns -> let (next:_) = dropWhile (== prev) ns in go (c+1) next n

-- Since this is a cartesian grid, the maximum distance from the start will be
-- exactly half the length of the loop.
maxDistanceFromStart :: Parsed -> Int
maxDistanceFromStart p = (loopLength p) `div` 2

-- The Shoelace formula is an easy to understand (if you see it visually) way to
-- compute the area of a cartesian polygon. The gist is that if we can consider
-- the polygon as a composed of trapezoids defined by each consecutive pair of
-- vertices. Some of these pairs will have a positive contribution, others will
-- have a negative contribution. So if we sum them up, we'll get the area.
--
-- The resultant value can be negative depending on the direction we go, thus we
-- take the absolute value to ignore that issue.
shoelaceArea :: Parsed -> Int
shoelaceArea Parsed { start, neighbours, vertices } = go 0 start start start
  where
    go s lastVertex prev n | n == start && prev /= start
      = (abs (s + shoelace lastVertex n)) `div` 2
    go s lastVertex prev n
      = let (next:_) = filter (/= prev) (fromJust (M.lookup n neighbours)) in
        if S.member n vertices
          then go (s + shoelace lastVertex n) n n next
          else go s lastVertex n next
    shoelace (y, x) (y', x') = (y + y') * (x - x')

-- Pick's formula gives us the way to relate the area of a cartesian polygon (a
-- polygon with integer coordinates for all its vertices) in terms of the number
-- of integer points within and on it.
--
-- Let i be the number of integer points interior to the polygon. This is what
-- we wish to find.
--
-- Let a be the area of the polygon. This we can find using the shoelaceArea
-- function above.
--
-- Let b be the number of integer points on boundary. This is the path length,
-- given by the loopLength function above.
--
-- Then, the area of this polygon is
--
--     A = i + (b/2)  - 1
--
-- Or, for us, the number of interior points is
--
--     i = A - (b/2) + 1
--
interiorPoints :: Int -> Int -> Int
interiorPoints area b = area - (b `div` 2) + 1

p1, p2 :: Parsed -> Int
p1 = maxDistanceFromStart
p2 p = interiorPoints (shoelaceArea p) (loopLength p)
