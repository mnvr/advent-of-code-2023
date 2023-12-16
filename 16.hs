import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Ix = (Int, Int)
data Grid = Grid { chars :: M.Map Ix Char, mi :: Ix }

parse :: String -> Grid
parse = mkC . concatMap (uncurry f) . zip [0..] . lines
  where f y = map (uncurry g) . zip [0..]
          where g x = ((x, y),)
        mkC xs = Grid (M.fromList xs) (fst $ last xs)

p1 :: Grid -> Int
p1 = (`energized` ((0, 0), R))

p2 :: Grid -> Int
p2 contraption = maximum $ map (energized contraption) $ edges contraption

data Direction = R | L | U | D deriving (Ord, Eq)
type Beam = (Ix, Direction)

energized :: Grid -> Beam -> Int
energized Grid { chars, mi = (mx, my) } start =
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

    until b '|' | isHorizontal b = ([b], [splitU b, splitD b])
    until b '-' | isVertical b = ([b], [splitL b, splitR b])
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
    until b _ =
      let n = step b in
      if inBounds n then let (ray, beams) = until n (char n) in (b : ray, beams)
      else ([b], [])

    inBounds ((x, y), _) = x >= 0 && y >= 0 && x <= mx && y <= my
    char b = fromJust $ M.lookup (fst b) chars
    isHorizontal (_, d) = d == L || d == R
    isVertical = not . isHorizontal
    step ((x, y), R) = ((x + 1, y), R)
    step ((x, y), L) = ((x - 1, y), L)
    step ((x, y), U) = ((x, y - 1), U)
    step ((x, y), D) = ((x, y + 1), D)
    splitL ((x, y), _) = ((x - 1, y), L)
    splitR ((x, y), _) = ((x + 1, y), R)
    splitU ((x, y), _) = ((x, y - 1), U)
    splitD ((x, y), _) = ((x, y + 1), D)
    reflectU ((x, y), _) = ((x, y - 1), U)
    reflectD ((x, y), _) = ((x, y + 1), D)
    reflectL ((x, y), _) = ((x - 1, y), L)
    reflectR ((x, y), _) = ((x + 1, y), R)

edges :: Grid -> [Beam]
edges Grid { mi = (mx, my) } = concat [
  [b | y <- [0..my], b <- [((0, y), R), ((mx, y), L)]],
  [((x, 0),  D) | x <- [0..mx]],
  [((x, my),  U) | x <- [0..mx]]]
