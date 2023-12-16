import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust, fromMaybe)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Ix = (Int, Int)
data Contraption = Contraption { grid :: M.Map Ix Char, mi :: Ix }

parse :: String -> Contraption
parse = mkC . concatMap (uncurry f) . zip [0..] . lines
  where f y = map (uncurry g) . zip [0..]
          where g x = ((x, y),)
        mkC xs = Contraption (M.fromList xs) (fst $ last xs)

p1 :: Contraption -> Int
p1 = (`energized` ((0, 0), R))

p2 :: Contraption -> Int
p2 contraption = maximum $ map (energized contraption) $ edgeBeams contraption

data Direction = R | L | U | D deriving (Ord, Eq)
type Beam = (Ix, Direction)

energized :: Contraption -> Beam -> Int
energized Contraption { grid, mi = (mx, my) } start =
  trace 0 S.empty S.empty [start]
  where
    trace :: Int -> S.Set Ix -> S.Set Beam -> [Beam] -> Int
    trace c _ _ [] = c
    trace c m visited (b@(t, d):bs)
      | S.member b visited = trace c m visited bs
      | otherwise =
        let ch = fromJust $ M.lookup t grid
        in trace (c + (if S.member t m then 0 else 1))
                 (S.insert t m)
                 (S.insert b visited)
                 (filter inBounds (next b ch) ++ bs)

    next b '.' = [step b]
    next b '-'
      | isHorizontal b = [step b]
      | otherwise = [splitL b, splitR b]
    next b '|'
      | isVertical b = [step b]
      | otherwise = [splitU b, splitD b]
    next b@(_, d) '\\'
      | d == R = [reflectD b]
      | d == L = [reflectU b]
      | d == U = [reflectL b]
      | d == D = [reflectR b]
    next b@(_, d) '/'
      | d == R = [reflectU b]
      | d == L = [reflectD b]
      | d == U = [reflectR b]
      | d == D = [reflectL b]

    inBounds ((x, y), _) = x >= 0 && y >= 0 && x <= mx && y <= my
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

edgeBeams :: Contraption -> [Beam]
edgeBeams Contraption { mi = (mx, my) } = concatMap line [0..my]
  where line y | y == 0  = map (\x -> ((x, y), D)) [0..mx]
               | y == my = map (\x -> ((x, y), U)) [0..mx]
               | otherwise = [((0, y), R), ((mx, y), L)]
