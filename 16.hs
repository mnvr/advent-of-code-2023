import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust, fromMaybe)
import Control.Arrow ((&&&))

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

type Ix = (Int, Int)
data Contraption = Contraption { grid :: M.Map Ix Char, mi :: Ix }
  deriving Show
type Tiles = S.Set Ix

parse :: String -> Contraption
parse = mkC . concatMap (uncurry f) . zip [0..] . lines
  where f y = map (uncurry g) . zip [0..]
          where g x = ((x, y),)
        mkC xs = Contraption (M.fromList xs) (fst $ last xs)

p1 :: Contraption -> Int
p1 = (`energized` ((0, 0), R))

energized :: Contraption -> Beam -> Int
energized contraption = S.size . flood contraption

p2 :: Contraption -> Int
p2 contraption = maximum $ map (energized contraption) $ edgeBeams contraption

data Direction = R | L | U | D deriving (Ord, Eq, Show)
type Beam = (Ix, Direction)

flood :: Contraption -> Beam -> Tiles
flood ctrp@Contraption { grid, mi = (mx, my) } start = go S.empty [start]
  where
    -- go through all the pending beams that we haven't traced yet
    go :: Tiles -> [Beam] -> Tiles
    go m [] = m
    go m (b:beams) = go (trace m S.empty [b]) beams
    -- trace a single beam all the way through, splitting it if needed, until it
    -- can't visit any more new tiles.
    trace m _ [] = m
    trace m visited (b@(t, d):bs)
      | S.member b visited = trace m visited bs
      | otherwise =
        let c = fromJust $ M.lookup t grid
        in trace (S.insert t m)
                 (S.insert b visited)
                 (filter inBounds (next b c) ++ bs)

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

    isHorizontal (_, d) = d == L || d == R
    isVertical = not . isHorizontal
    inBounds = (\((x, y), _) -> x >= 0 && y >= 0 && x <= mx && y <= my)
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

    incr (Just i) = Just (i + 1)
    incr Nothing = Just 1


edgeBeams :: Contraption -> [Beam]
edgeBeams Contraption { mi = (mx, my) } = concatMap line [0..my]
  where line y | y == 0  = map (\x -> ((x, y), D)) [0..mx]
               | y == my = map (\x -> ((x, y), U)) [0..mx]
               | otherwise = [((0, y), R), ((mx, y), L)]
