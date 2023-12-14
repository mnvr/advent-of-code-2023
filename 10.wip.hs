import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Control.Arrow ((&&&))
import Debug.Trace (trace)

main :: IO ()
-- main = interact $ (++ "\n") . show . (p1 &&& p2) . parse
main = interact $ (++ "\n") . show . p2 . parse

type Node = (Int, Int)

data Parsed = Parsed {
  start :: Node,
  neighbours :: M.Map Node [Node],
  charMap :: M.Map Node Char,
  dm :: M.Map Node Int,
  ny :: Int,
  nx :: Int
  }

parse :: String -> Parsed
parse = mkParsed . chunks . lines
  where
    mkParsed ck@((h, _, _):_) = let (s, nb) = ensureStart (neighbours ck)
      in Parsed { start = s, neighbours = nb,
                  charMap = chars ck,
                  dm = dist s nb,
                  ny = length ck, nx = length h }

    chunks ls = let g = ground ls in zip3 (g : ls) ls (drop 1 ls ++ [g])
    ground (h:_) = length h `replicate` '.'

    chars ck = foldl f M.empty (enum ck) where
      f m (y, ck@(_, c, _)) = foldl g m (enum c) where
          g m (x, c) = M.insert (y, x) c m

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

loopArea :: Node -> M.Map Node [Node] -> M.Map Node Char -> Int -> Int
loopArea start neighbours chars len = go 0 start start start
  where
    -- go a n since prev | n == start && prev /= start = let z = (abs (a + area since n)) `div` 2 in trace ("returning " ++ show z) z
    go a n since prev | n == start && prev /= start = let z = (abs (a + area since n) - len + 3) `div` 2 in trace ("returning " ++ show z) z
    go a n since prev = case M.lookup n neighbours of
      Just ns -> trace ("at " ++ show (ch n) ++ " - " ++ show a ++ " " ++ show n ++ " " ++ show since ++ " isEdge " ++ show (isEdge n)) $ let (next:_) = filter (/= prev) ns in
        if isEdge n then go (a + area since n) next n n else go a next since n
    area (y, x) (y', x') = let z = area1 (y, x) (y', x') in trace ("area " ++ show (x, y) ++ " " ++ show (x', y') ++ " : "++ show (y + y') ++ " * " ++ show (x - x') ++ " = " ++ show z) z
    isEdge n = case M.lookup n chars of
      Just c -> c `elem` "SFLJ7"
      _ -> False
    ch n = fromJust $ M.lookup n chars

    area1 (y, x) (y', x') = (y + y') * (x - x')
    -- area2 (y, x) (y', x') = y'  * (x - x')


dist :: Node -> M.Map Node [Node] -> M.Map Node Int
dist start neighbours = relax (M.singleton start 0) [start]
  where
    relax dm [] = dm
    relax dm (key : q) = case (M.lookup key dm, M.lookup key neighbours) of
        (Just d, Just ns) -> uncurry relax $ foldl (relaxNeighbour d) (dm, q) ns
    relaxNeighbour d (dm, q) n = case M.lookup n dm of
        Just nd | nd <= d + 1 -> (dm, q)
        _ -> (M.insert n (d + 1) dm, q ++ [n])

p1 :: Parsed -> Int
p1 Parsed { start, neighbours } = maxDist start neighbours -- maximum $ M.elems dm

-- p2 :: Parsed -> Int
p2 Parsed { start, neighbours, charMap } =
   let len = loopLength start neighbours
       a = loopArea start neighbours charMap len
   in trace ("loop area " ++ show a ++ " len " ++ show len) (a)

data Grid = Grid { gm :: M.Map Node Char, gny :: Int, gnx :: Int }

mkGrid :: [String] -> Int -> Int -> Grid
mkGrid ls ny nx = Grid { gm = mkGridMap ls, gny = ny, gnx = nx }

p2' :: Parsed -> Int
p2' pr = countEmpty $ gm $ collapse $ mask eg reachable
  where
    eg = expand pr
    reachable = dist (0, 0) (inverseMap eg)
    countEmpty = length . M.elems . M.filter (== '?')

mkGridMap :: [String] -> M.Map Node Char
mkGridMap = foldl f M.empty . enum
  where
    f m (y, row) = foldl (g y) m (enum row)
    g y m (x, c) = M.insert (y, x) c m

expand :: Parsed -> Grid
expand Parsed { neighbours, dm, ny, nx } =
  Grid { gm = mkGridMap expandLines, gny = eny, gnx = enx }
  where
    keys = M.keys dm
    eny = 3 * (ny + 2)
    enx = 3 * (nx + 2)
    expandLines =
      addBoundaryLines $ concatMap (addBoundary . expandRow) [0..ny-1]
    expandRow y =
      foldr (\(c1, c2, c3) ([l1, l2, l3]) -> [c1 ++ l1, c2 ++ l2, c3 ++ l3])
      [[], [], []] (expandRow' y)
    expandRow' y = map (\x -> expandCell (y, x)) [0..nx-1]
    expandCell key
      | key `elem` keys = expandCell' key (M.lookup key neighbours)
      | otherwise = ("???", "???", "???")
    expandCell' key@(y, x) (Just [n1, n2])
      | n1 == (y, x - 1) && n2 == (y, x + 1) = ("???", "---", "???")
      | n1 == (y - 1, x) && n2 == (y, x - 1) = ("?|?", "-J?", "???")
      | n1 == (y + 1, x) && n2 == (y, x - 1) = ("???", "-7?", "?|?")
      | n1 == (y + 1, x) && n2 == (y, x + 1) = ("???", "?F-", "?|?")
      | n1 == (y - 1, x) && n2 == (y, x + 1) = ("?|?", "?L-", "???")
      | n1 == (y - 1, x) && n2 == (y + 1, x) = ("?|?", "?|?", "?|?")
    boundaryLine = enx `replicate` '#'
    addBoundary = map (\s -> "###" ++ s ++ "###")
    addBoundaryLines ls = let bs = 3 `replicate` boundaryLine in bs ++ ls ++ bs

-- A neighbour map that connects non-pipe nodes
inverseMap :: Grid -> M.Map Node [Node]
inverseMap Grid { gm, gny, gnx } = go
  where
    go = foldl f M.empty [0..gny-1]
    f m y = foldl g m [0..gnx-1]
      where g m x | isEmpty (y, x) = foldl alterMap m (neighbors (y, x) m)
                  | otherwise = m
                      where alterMap m nk =  M.alter af (y, x) m
                              where af Nothing = Just [nk]
                                    af (Just xs) = Just (nk : xs)
    neighbors (y, x) m = catMaybes [
      ifEmpty (y, x - 1), ifEmpty (y - 1, x),
      ifEmpty (y, x + 1), ifEmpty (y + 1, x)]
    ifEmpty key | isEmpty key = Just key
                | otherwise = Nothing
    isInBounds (y, x) = y >= 0 && y < gny && x >= 0 && x < gnx
    isEmpty key = let ch = M.lookup key gm in
      isInBounds key && ((ch == Just '?') || (ch == Just '#'))

mask :: Grid -> M.Map Node Int -> Grid
mask Grid { gm, gny, gnx } dm = Grid { gm = go, gny, gnx }
  where
    go = M.mapWithKey f gm
    f key a = if M.member key dm then '#' else a

collapse :: Grid -> Grid
collapse Grid { gm, gny, gnx } = Grid { gm = cm, gny = cny, gnx = cnx }
  where
    cny = (gny `div` 3) - 2
    cnx = (gnx `div` 3) - 2
    cm = M.foldrWithKey f M.empty gm
    f key@(y, x) ch m
      | isNotBoundary key && y `mod` 3 == 0 && x `mod` 3 == 0 =
         M.insert ((y - 3) `div` 3, (x - 3) `div` 3) (g key gm) m
      | otherwise = m
    isNotBoundary (y, x) = y > 2 && y < gny - 3 && x > 2 && x < gnx - 3
    g (y, x) m = fromJust $ M.lookup (y+1, x+1) m
