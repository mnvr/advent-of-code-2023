import Data.Bifunctor (first, second)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Control.Arrow ((&&&))
import Debug.Trace
import Data.List (intersperse)

-- WIP!!
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

p2a inp = concat $ intersperse "\n" $ map scan rows
  where
    dm = dist inp
    keys = M.keys dm
    rows = range $ map fst $ keys
    cols = range $ map snd $ keys
    scan y = (\(_,_,ss) -> concatMap show (reverse ss)) $ foldl f (Outside, ' ', []) cols
      where
        isOnLoop x = (y, x) `elem` keys
        f (state, prevC, ss) x =
            let ch = if isOnLoop x then '|' else ' '
                state' = move state prevC ch (M.lookup (y, x) dm)
            in (state', ch, state':ss)

range :: [Int] -> [Int]
range xs = [minimum xs..maximum xs]

data State = Outside | Boundary1 Int | Boundary2 Int | Inside

move :: State -> Char -> Char -> Maybe Int -> State
move Outside ' ' '|' (Just d) = Boundary1 d
move Outside _ _ _ = Outside
move (Boundary1 _) _ ' ' _ = Inside
move (Boundary1 _) _ '|' (Just d) = Boundary2 d
move (Boundary2 _) _ '|' (Just d) = Boundary2 d
move (Boundary2 _) _ ' ' _ = Outside
move Inside _ ' ' _ = Inside
move Inside _ '|' (Just d) = Boundary2 d -- ?

instance Show State where
  show Outside = " . "
  show (Boundary1 d) = if d < 10 then (" " ++ show d ++ " ") else (show d ++ " ")
  show (Boundary2 d) = if d < 10 then (" " ++ show d ++ " ") else (show d ++ " ")
  show Inside = "   "

p2Pre inp@(_, neighbors) = expand
  where
    dm = dist inp
    keys = M.keys dm
    rows = range $ map fst $ keys
    cols = range $ map snd $ keys
    ncols = (\ys -> maximum ys - minimum ys) $ map snd $ keys
    empty = replicate (2 * (ncols + 2)) '#'
    ce = (\ss -> [empty, "\n"] ++ ss ++ [empty])
    re = (\s -> "#" ++ s ++ "#")
    expand = concat $ ce $ map (\(a,b)-> (re a) ++ "\n" ++ (re b) ++ "\n") expand'
    expand' :: [(String, String)]
    expand' = map expandRow rows
    expandRow :: Int -> (String, String)
    expandRow row = foldr (\(ta, tb) (a, b) -> (a ++ ta, b ++ tb)) ([], []) $ expandRow' row
    expandRow' :: Int -> [(String, String)]
    expandRow' row = map (exp row) cols
    exp y x | (y, x) `elem` keys = let key = (y, x) in
              expPipe key (M.lookup key neighbors)
            | otherwise = ("ee", "ee")
    expPipe key@(y, x) (Just (n1, n2))
      | n1 == (y, x - 1) && n2 == (y, x + 1) = ("--",
                                                "ee")
      | n1 == (y - 1, x) && n2 == (y, x - 1) = ("|e",
                                                "L/")
      | n1 == (y + 1, x) && n2 == (y, x - 1) = ("F\\",
                                                "|e")
      | n1 == (y - 1, x) && n2 == (y, x + 1) = ("|e",
                                                "Je")
      | n1 == (y + 1, x) && n2 == (y, x + 1) = ("7e",
                                                "|e")
      | n1 == (y - 1, x) && n2 == (y + 1, x) = ("|e",
                                                "|e")
      | otherwise = ("..", "..")

p2 inp = grid
  where grid = p2Pre inp
