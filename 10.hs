import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Control.Arrow ((&&&))
import Debug.Trace
import Data.List (intersperse)

main :: IO ()
-- main = interact $ (++ "\n") . show  . p2 . parse
main = interact $ (++ "\n") . p2 . parse

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

p1 :: (Node, M.Map Node (Node, Node)) -> Int
p1 = maximum . M.elems . dist

dist :: (Node, M.Map Node (Node, Node)) -> (M.Map Node Int)
dist (start, neighbours) = relax (distanceMap start) [start]
  where
    distanceMap s = M.singleton start 0
    relax :: M.Map Node Int -> [Node] -> M.Map Node Int
    relax dm [] = dm
    relax dm (key:q) = case (M.lookup key dm, M.lookup key neighbours) of
        (Just dist, Just (n1, n2)) ->
            let (dm', q') = relaxNeighbour n1 dm q dist in
                (let (dm'', q'') = relaxNeighbour n2 dm' q' dist in relax dm'' q'')
    relaxNeighbour nn dm q dist = case M.lookup nn dm of
        Nothing -> (M.insert nn (dist + 1) dm, q ++ [nn])
        Just d -> if dist + 1 < d then (M.insert nn (dist + 1) dm, q ++ [nn]) else (dm, q)

-- p2 inp = concat $ intersperse "\n" $ map scan [fst rows..snd rows]
p2 inp = concat $ intersperse "\n" $ map (\xs -> concat (map show xs)) $ map scan [fst rows..snd rows]
  where
    dm = dist inp
    keys = M.keys dm
    rows = (minimum &&& maximum) $ map fst keys
    cols = (minimum &&& maximum) $ map snd keys
    scan y = (\(_,i,_,_,_,_,_,_) -> reverse i) $ foldl ff ("", [], 0, ' ', 0, True, 0, 1) [fst cols..snd cols]
      where
        onLoop x = (y, x) `elem` keys
        initIsIn = if onLoop 0 then 1 else 0

        isEdge '|' x | onLoop x = False
        isEdge '|' x | otherwise = True
        isEdge ' ' x | onLoop x = True
        isEdge ' ' x | otherwise = False

        edgeD '|' x | onLoop x = \z -> if z == 1 then 0 else z
        edgeD '|' x | otherwise = (subtract 1)
        edgeD ' ' x | onLoop x = (+1)
        edgeD ' ' x | otherwise = (+0)

        sh True = 1
        sh False = 0

        -- ff :: (String, Int, Char) -> Int -> (String, Int, Char)
        ff (s, is, i, prevC, isIn, isOutside, ec, dt) x =
            let isL = onLoop x
                isE = isEdge prevC x
                toggle x = if x == 0 then 1 else 0
                c = if onLoop x then '|' else ' '
                j = if isEdge prevC x then i + 1 else i
                isIn' = isIn
                isOutside' = if isOutside && isE && isL then False else if not isOutside && isE && not isL then True else isOutside
                -- ec' = if prevC == ' ' && isL then ec + 1 else if prevC == '|'
                -- && not isL then ec - 1 else ec
                ec' = if isL then ec + dt else ec
                dt' = if isL then dt * (-1) else dt
                z = if isOutside' && ec' == 1 then 1 else 0
                -- v = if ec == 1 && isOutside then 1 else 0
            in (c : s, z : is, j, c, isIn', isOutside', ec',dt')


