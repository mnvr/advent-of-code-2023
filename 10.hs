import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Node = (Int, Int)

data Parsed = Parsed {
  start :: Node,
  neighbours :: M.Map Node [Node],
  dm :: M.Map Node Int,
  ny :: Int,
  nx :: Int
  }

parse :: String -> Parsed
parse = mkParsed . chunks . lines
  where
    mkParsed ck@((h, _, _):_) = let (s, nb) = ensureStart (neighbours ck)
      in Parsed { start = s, neighbours = nb,
                  dm = dist s nb,
                  ny = length ck, nx = length h }

    chunks ls = let g = ground ls in zip3 (g : ls) ls (drop 1 ls ++ [g])
    ground (h:_) = length h `replicate` '.'

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
p1 Parsed { dm } = maximum $ M.elems dm

data Grid = Grid { gm :: M.Map Node Char, gny :: Int, gnx :: Int }

mkGrid :: [String] -> Int -> Int -> Grid
mkGrid ls ny nx = Grid { gm = mkGridMap ls, gny = ny, gnx = nx }

p2 :: Parsed -> Int
p2 pr = countEmpty $ gm $ collapse $ mask eg reachable
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
