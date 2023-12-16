import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust)

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

type Ix = (Int, Int)
data Contraption = Contraption { grid :: M.Map Ix Char, mi :: Ix }
  deriving Show
type Tiles = M.Map Ix Int

parse :: String -> Contraption
parse = mkC . concatMap (uncurry f) . zip [0..] . lines
  where f y = map (uncurry g) . zip [0..]
          where g x = ((x, y),)
        mkC xs = Contraption (M.fromList xs) (fst $ last xs)

p1 = flood

data Direction = R | L | U | D deriving Eq
type Beam = (Ix, Direction)

flood :: Contraption -> Tiles
flood Contraption { grid, mi = (mx, my) } =
    go M.empty [((0, 0), R)] -- introduce a beam at (0,0), going right
  where
    -- go through all the pending beams that we haven't traced yet
    go :: Tiles -> [Beam] -> Tiles
    go m [] = m
    go m (b:beams) = go (trace m S.empty [b]) beams
    -- trace a single beam all the way through, splitting it if needed, until it
    -- can't visit any more new tiles.
    trace m _ [] = m
    trace m visited (b@(t, d):bs)
      | S.member t visited = trace m visited bs
      | otherwise =
        let c = fromJust $ M.lookup t grid
            m' = M.alter incr t m
            v' = (S.insert t visited)
            recurse = trace m' v'
        in case c of
          '.' -> recurse bs
          '-' | isHorizontal b -> recurse bs
              | otherwise -> recurse (bs ++ hsplit t)
          '|' | isVertical b -> recurse bs
              | otherwise -> recurse (bs ++ vsplit t)
          '\\' | d == R -> recurse (bs ++ reflectD t)
               | d == L -> recurse (bs ++ reflectU t)
               | d == U -> recurse (bs ++ reflectL t)
               | d == D -> recurse (bs ++ reflectR t)
          '/'  | d == R -> recurse (bs ++ reflectU t)
               | d == L -> recurse (bs ++ reflectD t)
               | d == U -> recurse (bs ++ reflectR t)
               | d == D -> recurse (bs ++ reflectL t)
    tile (t, _) = t
    isHorizontal (_, d) = d == L || d == R
    isVertical = not . isHorizontal
    hsplit (x, y) = prune [((x - 1, y), L), ((x + 1, y), R)]
    vsplit (x, y) = prune [((x, y - 1), U), ((x, y + 1), D)]
    prune = filter (\((x, y), _) -> x > 0 && y > 0 && x <= mx && y <= my )
    reflectU (x, y) = prune [((x, y - 1), U)]
    reflectD (x, y) = prune [((x, y + 1), D)]
    reflectL (x, y) = prune [((x - 1, y), L)]
    reflectR (x, y) = prune [((x + 1, y), R)]

    incr (Just i) = Just (i + 1)
    incr Nothing = Just 1
