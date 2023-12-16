import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromJust, fromMaybe)
import Control.Arrow ((&&&))

import Debug.Trace qualified as T

t_trace x y = y

main :: IO ()
main = interact $ (++ "\n") . show . (p1 &&& p2) . parse

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
-- p1 c = let ft = (flood c) in (showTiles c ft ++ "\n" ++ show (length $ M.keys ft))
p1 = (`energized` ((0, 0), R))

energized :: Contraption -> Beam -> Int
energized contraption = length . M.keys . flood contraption

p2 :: Contraption -> Int
p2 contraption = maximum $ map (energized contraption) $ edgeBeams contraption

data Direction = R | L | U | D deriving (Ord, Eq, Show)
type Beam = (Ix, Direction)

-- type Mk = Tiles (S.Set )
flood :: Contraption -> Beam -> Tiles
flood ctrp@Contraption { grid, mi = (mx, my) } start = go M.empty [start]
  where
    -- go through all the pending beams that we haven't traced yet
    go :: Tiles -> [Beam] -> Tiles
    go m [] = t_trace (showTiles ctrp m) $ m
    go m (b:beams) = t_trace (showTiles ctrp m) $ go (trace m S.empty [b]) beams
    -- trace a single beam all the way through, splitting it if needed, until it
    -- can't visit any more new tiles.
    trace m _ [] = m
    trace m visited (b@(t, d):bs)
      | S.member b visited = t_trace ("already visited " ++ show b) $ trace m visited bs
      | otherwise = t_trace ("visiting " ++ show t) $
        let c = fromJust $ M.lookup t grid
            m' = M.alter incr t m
            ns = case c of
              '.' -> [step b]
              '-' | isHorizontal b -> [step b]
                  | otherwise -> [splitL b, splitR b]
              '|' | isVertical b -> [step b]
                  | otherwise -> [splitU b, splitD b]
              '\\' | d == R -> [reflectD' b]
                   | d == L -> [reflectU' b]
                   | d == U -> [reflectL' b]
                   | d == D -> [reflectR' b]
              '/'  | d == R -> [reflectU' b]
                   | d == L -> [reflectD' b]
                   | d == U -> [reflectR' b]
                   | d == D -> [reflectL' b]
            fns = filter (\b' -> inBounds b') ns
        in case fns of
          [] -> trace m' visited bs
          [p] -> trace m' (S.insert b visited) (p:bs)
          [p,q] -> trace m' (S.insert b visited) (p:q:bs)

    tile (t, _) = t
    isHorizontal (_, d) = d == L || d == R
    isVertical = not . isHorizontal
    hsplit (x, y) = prune [((x - 1, y), L), ((x + 1, y), R)]
    vsplit (x, y) = prune [((x, y - 1), U), ((x, y + 1), D)]
    splitL ((x, y), _) = ((x - 1, y), L)
    splitR ((x, y), _) = ((x + 1, y), R)
    splitU ((x, y), _) = ((x, y - 1), U)
    splitD ((x, y), _) = ((x, y + 1), D)
    hsplit2 b bs =
      let l = splitL b
          r = splitR b
      in if inBounds l && inBounds r then l:r:bs
         else if inBounds l then l:bs
         else if inBounds r then r:bs
         else bs
    vsplit2 b bs =
      let u = splitU b
          d = splitD b
      in if inBounds u && inBounds d then u:d:bs
         else if inBounds u then u:bs
         else if inBounds d then d:bs
         else bs

    inBounds = (\((x, y), _) -> x >= 0 && y >= 0 && x <= mx && y <= my)
    prune = filter inBounds
    next b bs = let n = step b in if inBounds n then n:bs else bs
    step ((x, y), R) = ((x + 1, y), R)
    step ((x, y), L) = ((x - 1, y), L)
    step ((x, y), U) = ((x, y - 1), U)
    step ((x, y), D) = ((x, y + 1), D)
    reflectU' ((x, y), _) = ((x, y - 1), U)
    reflectD' ((x, y), _) = ((x, y + 1), D)
    reflectL' ((x, y), _) = ((x - 1, y), L)
    reflectR' ((x, y), _) = ((x + 1, y), R)

    reflectU b bs = let b' = reflectU' b in if inBounds b' then b' : bs else bs
    reflectD b bs = let b' = reflectD' b in if inBounds b' then b' : bs else bs
    reflectL b bs = let b' = reflectL' b in if inBounds b' then b' : bs else bs
    reflectR b bs = let b' = reflectR' b in if inBounds b' then b' : bs else bs

    incr (Just i) = Just (i + 1)
    incr Nothing = Just 1

showTiles :: Contraption -> Tiles -> String
showTiles Contraption { mi = (mx, my) } t = unlines (map line [0..my])
  where line y = concatMap g [0..mx]
          where g x = show $ fromMaybe 0 $ M.lookup (x, y) t

edgeBeams :: Contraption -> [Beam]
edgeBeams Contraption { mi = (mx, my) } = concatMap line [0..my]
  where line y | y == 0  = map (\x -> ((x, y), D)) [0..mx]
               | y == my = map (\x -> ((x, y), U)) [0..mx]
               | otherwise = [((0, y), R), ((mx, y), L)]
