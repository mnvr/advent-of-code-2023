import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust, fromMaybe)

import Debug.Trace qualified as T

main :: IO ()
main = interact $ (++ "\n") . p1 . parse

type Ix = (Int, Int)
data Contraption = Contraption { grid :: M.Map Ix Char, mi :: Ix }
  deriving Show
type Tiles = M.Map Ix Int

parse :: String -> Contraption
parse = mkC . concatMap (uncurry f) . zip [0..] . lines
  where f y = map (uncurry g) . zip [0..]
          where g x = ((x, y),)
        mkC xs = Contraption (M.fromList xs) (fst $ last xs)

-- p1 = length . M.keys . flood
p1 c = let ft = (flood c) in (showTiles c ft ++ "\n" ++ show (length $ M.keys ft))


data Direction = R | L | U | D deriving (Ord, Eq, Show)
type Beam = (Ix, Direction)

flood :: Contraption -> Tiles
flood ctrp@Contraption { grid, mi = (mx, my) } =
    go M.empty [((0, 0), R)] -- introduce a beam at (0,0), going right
  where
    -- go through all the pending beams that we haven't traced yet
    go :: Tiles -> [Beam] -> Tiles
    go m [] = T.trace (showTiles ctrp m) $ m
    go m (b:beams) = T.trace (showTiles ctrp m) $ go (trace m S.empty [b]) beams
    -- trace a single beam all the way through, splitting it if needed, until it
    -- can't visit any more new tiles.
    trace m _ [] = m
    trace m visited (b@(t, d):bs)
      | S.member b visited = T.trace ("already visited " ++ show b) $ trace m visited bs
      | otherwise = T.trace ("visiting " ++ show t) $
        let c = fromJust $ M.lookup t grid
            m' = M.alter incr t m
            v' = (S.insert b visited)
            recurse z = T.trace ("recursing " ++ show z) $ trace m' v' z
        in case c of
          '.' -> recurse (step b ++ bs)
          '-' | isHorizontal b -> recurse (step b ++ bs)
              | otherwise -> recurse (bs ++ hsplit t)
          '|' | isVertical b -> recurse (step b ++ bs)
              | otherwise -> recurse (bs ++ vsplit t)
          '\\' | d == R -> recurse (reflectD t ++ bs)
               | d == L -> recurse (reflectU t ++ bs)
               | d == U -> recurse (reflectL t ++ bs)
               | d == D -> recurse (reflectR t ++ bs)
          '/'  | d == R -> recurse (reflectU t ++ bs)
               | d == L -> recurse (reflectD t ++ bs)
               | d == U -> recurse (reflectR t ++ bs)
               | d == D -> recurse (reflectL t ++ bs)
    tile (t, _) = t
    isHorizontal (_, d) = d == L || d == R
    isVertical = not . isHorizontal
    hsplit (x, y) = prune [((x - 1, y), L), ((x + 1, y), R)]
    vsplit (x, y) = prune [((x, y - 1), U), ((x, y + 1), D)]
    prune = filter (\((x, y), _) -> x >= 0 && y >= 0 && x <= mx && y <= my )
    step ((x, y), R) = prune [((x + 1, y), R)]
    step ((x, y), L) = prune [((x - 1, y), L)]
    step ((x, y), U) = prune [((x, y - 1), U)]
    step ((x, y), D) = prune [((x, y + 1), D)]
    reflectU (x, y) = prune [((x, y - 1), U)]
    reflectD (x, y) = prune [((x, y + 1), D)]
    reflectL (x, y) = prune [((x - 1, y), L)]
    reflectR (x, y) = prune [((x + 1, y), R)]

    incr (Just i) = Just (i + 1)
    incr Nothing = Just 1

showTiles :: Contraption -> Tiles -> String
showTiles Contraption { mi = (mx, my) } t = unlines (map line [0..my])
  where line y = concatMap g [0..mx]
          where g x = show $ fromMaybe 0 $ M.lookup (x, y) t
