import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Control.Arrow ((&&&))

main :: IO ()
-- main = interact $ (++ "\n") . show . (p1 &&& p2) . parse
-- main = interact $ (++ "\n") . show . p2 . parse
main = interact $ (++ "\n") . p2 . parse

type Node = (Int, Int)

data Parsed = Parsed {
  start :: Node,
  nm :: M.Map Node (Node, Node),
  dm :: M.Map Node Int,
  ny :: Int,
  nx :: Int
  }

parse :: String -> Parsed
parse = mkParsed . chunks . lines
  where
    mkParsed ck@((h, _, _):_) = let (s, nb) = ensureStart (neighbours ck)
      in Parsed { start = s, nm = nb, dm = mkDistanceMap s nb,
                  ny = length ck, nx = length h }
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
                _ -> (s, m)
    neighbour :: (String, String, String) -> Char -> (Int, Int) -> (Maybe (Int, Int), Maybe (Int, Int))
    neighbour (p, c, n) '|' k = (north p k, south n k)
    neighbour (p, c, n) '-' k = (west c k, east c k)
    neighbour (p, c, n) 'L' k = (north p k, east c k)
    neighbour (p, c, n) 'J' k = (north p k, west c k)
    neighbour (p, c, n) '7' k = (south n k, west c k)
    neighbour (p, c, n) 'F' k = (south n k, east c k)
    neighbour _ c k = error ("unexpected character " ++ show c ++ " at position " ++ show k)
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

mkDistanceMap :: Node -> M.Map Node (Node, Node) -> M.Map Node Int
mkDistanceMap start neighbours = relax (dm0 start) [start]
  where
    dm0 s = M.singleton start 0
    relax :: M.Map Node Int -> [Node] -> M.Map Node Int
    relax dm [] = dm
    relax dm (key:q) = case (M.lookup key dm, M.lookup key neighbours) of
        (Just dist, Just (n1, n2)) ->
            let (dm', q') = relaxNeighbour n1 dm q dist in
                (let (dm'', q'') = relaxNeighbour n2 dm' q' dist in relax dm'' q'')
    relaxNeighbour nn dm q dist = case M.lookup nn dm of
        Nothing -> (M.insert nn (dist + 1) dm, q ++ [nn])
        Just d -> if dist + 1 < d then (M.insert nn (dist + 1) dm, q ++ [nn]) else (dm, q)

p1 :: Parsed -> Int
p1 Parsed { dm } = maximum $ M.elems dm

data Grid = Grid { gm :: M.Map Node Char, gny :: Int, gnx :: Int }

mkGrid :: [String] -> Int -> Int -> Grid
mkGrid ls ny nx = Grid { gm = mkGridMap ls, gny = ny, gnx = nx }

instance Show Grid where
  show Grid { gm, gny, gnx } = unlines $ gridMapToLines gm gny gnx

gridMapToLines :: M.Map Node Char -> Int -> Int -> [String]
gridMapToLines gm ny nx = map makeRow [0..ny-1]
  where makeRow y = map (\x -> maybe '!' id (M.lookup (y, x) gm)) [0..nx-1]

showDistanceMap :: Grid -> M.Map Node Int -> String
showDistanceMap Grid { gm, gny, gnx } dm = unlines rows
  where
    rows = map row [0..gny-1]
    row y = concatMap item [0..gnx-1]
      where item x = case M.lookup (y, x) dm of
                       Nothing -> " .. "
                       Just d -> (if d < 10 then "  " ++ show d else " " ++ show d) ++ " "

-- p2 :: Parsed -> Int
-- p2 = mkNeighbourMap2 . expand -- countEmpty . gm . collapse . flood . expand
p2 pr = showDistanceMap eg $ mkDistanceMap2 (0,0) $ mkNeighbourMap2 $ eg -- countEmpty . gm . collapse . flood . expand
  where
    eg = expand pr
    -- oc = countEmpty . gm . collapse . flood . expand
    countEmpty = length . M.elems . M.filter (== '?')

-- create a new distance map that connects the non-pipe areas
mkNeighbourMap2 :: Grid -> M.Map Node [Node]
mkNeighbourMap2 Grid { gm, gny, gnx } = go
  where
    go = foldl f M.empty [0..gny-1]
    f m y = foldl g m [0..gnx-1]
      where g m x | M.member (y, x) m = m
                  | otherwise = foldl alterMap m (neighbors (y, x) m)
                      where alterMap m nk =  M.alter af (y, x) m
                              where af Nothing = Just [nk]
                                    af (Just xs) = Just (nk : xs)
    neighbors (y, x) m = catMaybes [
      ifEmpty (y, x - 1) m, ifEmpty (y - 1, x) m,
      ifEmpty (y, x + 1) m, ifEmpty (y + 1, x) m]
    ifEmpty key m | M.notMember key m && isInBounds key = Just key
                  | otherwise = Nothing
    isInBounds (y, x) = y >= 0 && y < gny && x >= 0 && x < gnx

mkDistanceMap2 :: Node -> M.Map Node [Node] -> M.Map Node Int
mkDistanceMap2 start neighbours = relax (dm0 start) [start]
  where
    dm0 s = M.singleton start 0
    relax :: M.Map Node Int -> [Node] -> M.Map Node Int
    relax dm [] = dm
    relax dm (key:q) = case (M.lookup key dm, M.lookup key neighbours) of
        (Just dist, Just ns) -> let (dm', q') = foldl f (dm, q) ns in relax dm' q'
            where f (dm', q) n = relaxNeighbour n dm' q dist
        -- (Just dist, Nothing) -> if key == (20, 20) then dm else error ("unexpected lookup result for key " ++ show key ++ " - " ++ show md ++ "  " ++ show mns)
        (md, mns) -> if key == (20, 20) then dm else error ("unexpected lookup result for key " ++ show key ++ " - " ++ show md ++ "  " ++ show mns)
    relaxNeighbour nn dm q dist = case M.lookup nn dm of
        Nothing -> (M.insert nn (dist + 1) dm, q ++ [nn])
        Just d -> if dist + 1 < d then (M.insert nn (dist + 1) dm, q ++ [nn]) else (dm, q)

mkGridMap :: [String] -> M.Map Node Char
mkGridMap = foldl f M.empty . enum
  where
    f m (y, row) = foldl (g y) m (enum row)
    g y m (x, c) = M.insert (y, x) c m
    enum = zip [0..]

expand :: Parsed -> Grid
expand Parsed { nm, dm, ny, nx } =
  Grid { gm = mkGridMap expandLines, gny = eny, gnx = enx }
  where
    keys = M.keys dm
    eny = 3 * (ny + 2)
    enx = 3 * (nx + 2)
    expandLines = addBoundaryLines $ concatMap (addBoundary . expandRow) [0..ny-1]
    expandRow y =
      foldr (\(c1, c2, c3) ([l1, l2, l3]) -> [c1 ++ l1, c2 ++ l2, c3 ++ l3])
      [[], [], []] (expandRow' y)
    expandRow' y = map (\x -> expandCell (y, x)) [0..nx-1]
    expandCell key | key `elem` keys = expandCell' key (M.lookup key nm)
                   | otherwise = ("???", "???", "???")
    expandCell' key@(y, x) (Just (n1, n2))
      | n1 == (y, x - 1) && n2 == (y, x + 1) = ("???", "---", "???")
      | n1 == (y - 1, x) && n2 == (y, x - 1) = ("?|?", "-J?", "???")
      | n1 == (y + 1, x) && n2 == (y, x - 1) = ("???", "-7?", "?|?")
      | n1 == (y + 1, x) && n2 == (y, x + 1) = ("???", "?F-", "?|?")
      | n1 == (y - 1, x) && n2 == (y, x + 1) = ("?|?", "?L-", "???")
      | n1 == (y - 1, x) && n2 == (y + 1, x) = ("?|?", "?|?", "?|?")
    boundaryLine = enx `replicate` '#'
    addBoundary = map (\s -> "###" ++ s ++ "###")
    addBoundaryLines ls = let bs = 3 `replicate` boundaryLine in bs ++ ls ++ bs

flood :: Grid -> Grid
flood = id -- Grid { gm, gny, gnx } = Grid { gm = go gm, gny = gny, gnx = gnx }
-- flood Grid { gm, gny, gnx } = Grid { gm = go gm, gny = gny, gnx = gnx }
  where
    go gm = case step gm of
      (0, gm') -> gm'
      (_, gm') -> go gm'
    step m = M.mapAccumWithKey f 0 m
      where
        f changed key '?' | any (=='#') (nbr key) = (changed + 1, '#')
        f changed key ch = (changed, ch)
        nbr (y,x) = catMaybes $ map (`M.lookup` m) [
          (y, x - 1), (y - 1, x), (y, x + 1), (y + 1, x)]

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
