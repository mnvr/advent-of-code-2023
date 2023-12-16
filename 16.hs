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

p1 = id

data Direction = R | L | U | D deriving Eq
type Beam = (Ix, Direction)

flood :: Contraption -> Tiles
flood Contraption { grid, mi = (mx, my) } =
    go M.empty [((0, 0), R)] -- introduce a beam at (0,0), going right
  where
    -- go through all the pending beams that we haven't traced yet
    go :: Tiles -> [Beam] -> Tiles
    go m [] = m
    go m (b:beams) = go (trace m [b] S.empty) beams
    -- trace a single beam all the way through, splitting it if needed, until it
    -- can't visit any more new tiles.
    trace m [] _ = m
    trace m (b:bs) visited
      | S.member (tile b) visited = trace m bs visited
      | otherwise =
        let t = (tile b)
            c = fromJust $ M.lookup t grid
            m' = M.alter incr t m
            v' = (S.insert t visited)
        in case c of
          '.' -> trace m' bs v'
          '-' | isHorizontal b -> trace m' bs v'
              | otherwise -> trace m' (bs ++ hsplit t) v'
          '|' | isVertical b -> trace m' bs v'
              | otherwise -> trace m' (bs ++ vsplit t) v'
    tile (t, _) = t
    isHorizontal (_, d) = d == L || d == R
    isVertical = not . isHorizontal
    hsplit (x, y) = prune [((x - 1, y), L), ((x + 1, y), R)]
    vsplit (x, y) = prune [((x, y - 1), U), ((x, y + 1), D)]
    prune = filter (\((x, y), _) -> x > 0 && y > 0 && x <= mx && y <= my )

    incr (Just i) = Just (i + 1)
    incr Nothing = Just 1
