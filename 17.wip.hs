import Data.Char (digitToInt)
import Data.Complex
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Debug.Trace

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

data Grid = Grid { gv :: [[Int]], mi :: Ix }
type Ix = Complex Float

parse :: String -> Grid
parse = mkG . map (map digitToInt) . lines
  where mkG xs@(h:_) = Grid xs (fromIntegral (length h - 1) :+ fromIntegral (length xs - 1))


p1 = (`path` (0 :+ 0))

path :: Grid -> Ix -> Int
path Grid { gv, mi } start =
    -- min (goStart (1 :+ 0)) (goStart (0 :+ 1))
    (goStart (1 :+ 0))
  where
    -- goStart h = go start h 0 (S.singleton (pair start)) 0
    goStart h = go start h 0 (S.empty) 0
    i :: Ix
    i = 0 :+ (-1)
    pair :: Ix -> (Int, Int)
    pair p = (floor $ realPart p, floor $ imagPart p)
    value :: Ix -> Int
    value = value' . pair
    value' (x, y) = gv !! y !! x
    inBounds :: Ix -> Bool
    inBounds = inBounds' (pair mi) . pair
    inBounds' :: (Int, Int) -> (Int, Int) -> Bool
    inBounds' (mx, my) (x, y) = x >= 0 && y >= 0 && x <= mx && y <= my
    go :: Ix -> Ix -> Int -> S.Set (Int, Int) -> Int -> Int
    go p _ _ _ c | p == mi = c
    -- go p _ _ visited c | S.member (pair p) visited = c
    go p h movesSinceTurn visited c =
        let visited' = S.insert (pair p) visited
            c' = c + value p
            ns = neighbours
        in trace ("go " ++ show p ++ " " ++ show h ++ " visited " ++ show (S.size visited) ++ " nodes") $
          if null ns then c else minimum (map (\(q, h', moves') -> go q h' moves' visited' c') ns)
      where
        valid :: Ix -> Bool
        valid q = inBounds q && (S.notMember (pair q) visited)
        ifValid :: (Ix, Ix, Int) -> Maybe (Ix, Ix, Int)
        ifValid (q, h', moves') = if valid q then Just (q, h', moves') else Nothing
        neighbours :: [(Ix, Ix, Int)]
        neighbours = catMaybes [
            if movesSinceTurn > 3 then Nothing else ifValid (p + h, h, movesSinceTurn + 1),
            ifValid (p + (i * h), (i * h), 0),
            ifValid (p + (-i * h), (-i * h), 0)]



