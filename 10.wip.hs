import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.List (intersperse)
import Debug.Trace (trace)

-- WIP!!
main :: IO ()
main = interact $ (++ "\n") . p2 . parse

type Node = (Int, Int)

data Parsed = Parsed {
  start :: Node,
  nm :: M.Map Node (Node, Node),
  ny :: Int,
  nx :: Int
  }

parse :: String -> Parsed
parse = mkParsed . chunks . lines
  where
    mkParsed ck@((h, _, _):_) = let (s, nb) = ensureStart (neighbours ck)
      in Parsed { start = s, nm = nb, ny = length ck, nx = length h }
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

p1 :: Parsed -> Int
p1 Parsed { start, nm } = p1' (start, nm)

p1' :: (Node, M.Map Node (Node, Node)) -> Int
p1' = maximum . M.elems . mkDistanceMap

mkDistanceMap :: (Node, M.Map Node (Node, Node)) -> (M.Map Node Int)
mkDistanceMap (start, neighbours) = relax (dm0 start) [start]
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

data Grid = Grid { gm :: M.Map Node Char, gny :: Int, gnx :: Int }

instance Show Grid where
  show Grid { gm, gny, gnx } = unlines $ gridMapToLines gm gny gnx

mkGrid :: [String] -> Int -> Int -> Grid
mkGrid ls ny nx = Grid { gm = mkGridMap ls, gny = ny, gnx = nx }

p2 :: Parsed -> String
p2 pr@Parsed { start, nm, ny, nx } = show eg -- gridLines gm eny enx
  -- let (log, flooded) = flood gm
      -- r = resultLog flooded
  -- in unlines $ gridLines gm eny enx -- $ log ++ gridLines flooded ny nx ++ r
     -- $ log ++ gridLines flooded ny nx ++ r
  where
    dm = mkDistanceMap (start, nm)
    og = reconstruct pr dm
    eg = expand pr dm
    -- grid = trace (show (ny, nx)) $ expand pr dm
    -- (eny, enx) = (2 * ny, 2 * nx)
    -- gm = mkGridMap grid
    -- resultLog m = ["inside " ++ show (countEmpty m)]
    -- countEmpty = length . M.elems . M.filter (== '.')

mkGridMap :: [String] -> M.Map Node Char
mkGridMap = foldl f M.empty . enum
  where
    f m (y, row) = foldl (g y) m (enum row)
    g y m (x, c) = M.insert (y, x) c m
    enum = zip [0..]

gridMapToLines :: M.Map Node Char -> Int -> Int -> [String]
gridMapToLines gm ny nx = map makeRow [0..ny-1]
  where makeRow y = map (\x -> maybe '!' id (M.lookup (y, x) gm)) [0..nx-1]

reconstruct :: Parsed -> M.Map Node Int -> Grid
reconstruct Parsed { nm, ny, nx } dm = mkGrid expandLines ny nx
  where
    keys = M.keys dm
    expandLines = map expandRow [0..ny-1]
    expandRow y = foldr (\l ls -> l:ls) [] (expandRow' y)
    expandRow' y = map (\x -> expandCell (y, x)) [0..nx-1]
    expandCell key | key `elem` keys = expandCell' key (M.lookup key nm)
                   | otherwise = '.'
    expandCell' key@(y, x) (Just (n1, n2))
      | n1 == (y, x - 1) && n2 == (y, x + 1) = '-'
      | n1 == (y - 1, x) && n2 == (y, x - 1) = 'J'
      | n1 == (y + 1, x) && n2 == (y, x - 1) = '7'
      | n1 == (y - 1, x) && n2 == (y, x + 1) = 'L'
      | n1 == (y + 1, x) && n2 == (y, x + 1) = 'F'
      | n1 == (y - 1, x) && n2 == (y + 1, x) = '|'

expand :: Parsed -> M.Map Node Int -> Grid
expand Parsed { nm, ny, nx } dm = mkGrid expandLines eny enx
  where
    keys = M.keys dm
    eny = 2 * ny + 4
    enx = 2 * nx + 4
    expandLines :: [String]
    expandLines = addBoundaryLines $ concatMap (addBoundary . expandRow) [0..ny-1]
    expandRow :: Int -> [String]
    expandRow y = foldr (\(c1, c2) ([l1, l2]) -> [c1 ++ l1, c2 ++ l2]) [[], []] (expandRow' y)
    expandRow' :: Int -> [(String, String)]
    expandRow' y = map (\x -> expandCell (y, x)) [0..nx-1]
    expandCell key | key `elem` keys = expandCell' key (M.lookup key nm)
                   | otherwise = (".?", "??")
    expandCell' key@(y, x) (Just (n1, n2))
      | n1 == (y, x - 1) && n2 == (y, x + 1) = ("--", "??")
      | n1 == (y - 1, x) && n2 == (y, x - 1) = ("|?", "J?")
      | n1 == (y + 1, x) && n2 == (y, x - 1) = ("7?", "|?")
      | n1 == (y - 1, x) && n2 == (y, x + 1) = ("|?", "L/")
      | n1 == (y + 1, x) && n2 == (y, x + 1) = ("F\\", "|?")
      | n1 == (y - 1, x) && n2 == (y + 1, x) = ("|?", "|?")
    boundaryLine = enx `replicate` '#'
    addBoundary = map (\s -> "##" ++ s ++ "##")
    addBoundaryLines ls = let bs = [boundaryLine, boundaryLine] in bs ++ ls ++ bs

    -- expandCell' key@(y, x) (Just (n1, n2))
    --   | n1 == (y, x - 1) && n2 == (y, x + 1) = ("--",
    --                                             "ee")
    --   | n1 == (y - 1, x) && n2 == (y, x - 1) = ("|e",
    --                                             "L/")
    --   | n1 == (y + 1, x) && n2 == (y, x - 1) = ("F\\",
    --                                             "|e")
    --   | n1 == (y - 1, x) && n2 == (y, x + 1) = ("|e",
    --                                             "Je")
    --   | n1 == (y + 1, x) && n2 == (y, x + 1) = ("7e",
    --                                             "|e")
    --   | n1 == (y - 1, x) && n2 == (y + 1, x) = ("|e",
    --                                             "|e")

flood :: M.Map Node Char -> ([String], M.Map Node Char)
flood m = ([], m) where -- go 0 m [] where
  go i m log = case step m of
    (0, m') -> (reverse (("iterations " ++ show i) : log), m')
    (changed, m') -> let r = remaining m'
                         msg = "changed " ++ show changed ++ "\tremaining " ++ show r
                     in go (i + 1) m' (msg :log)
  remaining = length . M.keys . M.filter (=='e')
  step :: M.Map Node Char -> (Int, M.Map Node Char)
  step m = M.mapAccumWithKey f 0 m
    where
        f :: Int -> Node -> Char -> (Int, Char)
        f changed key 'e' | any (== '#') (nbr key) = (changed + 1, '#')
        f changed _ ch = (changed, ch)
        nbr (y, x) = catMaybes $ map (`M.lookup` m) [
          (y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

markEmpty :: M.Map Node Char -> M.Map Node Char
markEmpty m = M.mapWithKey f m
  where
    f key 'e' | isEmptyBlock key = '.'
    f _ ch = ch
    isEmptyBlock (y, x) = even y && even x && all (=='e') nbrs
      where nbrs = catMaybes (map (`M.lookup` m) [(y+1,x), (y,x+1), (y+1,x+1)])
