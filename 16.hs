import Data.Array qualified as A
import Data.Set qualified as S
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Ix = (Int, Int)
data Grid = Grid { chars :: A.Array Ix Char, mx, my :: Int }

parse :: String -> Grid
parse = mkC . lines
  where
    mkC xs = let mx = length (xs !! 0) - 1
                 my = length xs - 1
                 axs = [((x, y), xs !! y !! x) | x <- [0..mx], y <- [0..my]]
             in Grid (A.array ((0, 0), (mx, my)) axs) mx my

p1 :: Grid -> Int
p1 = (`energized` ((0, 0), R))

p2 :: Grid -> Int
p2 grid = maximum $ map (energized grid) $ edges grid

data Direction = R | L | U | D deriving (Ord, Eq)
type Beam = (Ix, Direction)

energized :: Grid -> Beam -> Int
energized Grid { chars, mx, my } start =
  count $ trace S.empty [start]
  where
    count = S.size . S.map fst
    trace processed [] = processed
    trace processed (b:bs)
      | S.member b processed = trace processed bs
      | otherwise =
        let (ray, beams) = until b (char b)
        in trace (foldl (\s b -> S.insert b s) processed ray)
                 (bs ++ filter inBounds beams)

    until b '|' | isHorizontal b = ([b], splitV b)
    until b '-' | isVertical b =   ([b], splitH b)
    until b@(_, d) '\\'
      | d == R = ([b], [reflectD b])
      | d == L = ([b], [reflectU b])
      | d == U = ([b], [reflectL b])
      | d == D = ([b], [reflectR b])
    until b@(_, d) '/'
      | d == R = ([b], [reflectU b])
      | d == L = ([b], [reflectD b])
      | d == U = ([b], [reflectR b])
      | d == D = ([b], [reflectL b])
    until b _ = let n = step b in
      if inBounds n then let (ray, beams) = until n (char n) in (b : ray, beams)
      else ([b], [])

    inBounds ((x, y), _) = x >= 0 && y >= 0 && x <= mx && y <= my
    char b = chars A.! fst b
    isHorizontal (_, d) = d == L || d == R
    isVertical = not . isHorizontal
    step ((x, y), R) = ((x + 1, y), R)
    step ((x, y), L) = ((x - 1, y), L)
    step ((x, y), U) = ((x, y - 1), U)
    step ((x, y), D) = ((x, y + 1), D)
    splitH ((x, y), _) = [((x - 1, y), L), ((x + 1, y), R)]
    splitV ((x, y), _) = [((x, y - 1), U), ((x, y + 1), D)]
    reflectU ((x, y), _) = ((x, y - 1), U)
    reflectD ((x, y), _) = ((x, y + 1), D)
    reflectL ((x, y), _) = ((x - 1, y), L)
    reflectR ((x, y), _) = ((x + 1, y), R)

edges :: Grid -> [Beam]
edges Grid { mx, my } = concat [
  [b | y <- [0..my], b <- [((0, y), R), ((mx, y), L)]],
  [((x, 0),  D) | x <- [0..mx]],
  [((x, my),  U) | x <- [0..mx]]]
