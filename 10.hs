import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

parse = fixStart . neighbours . chunks . lines
  where
    chunks ls = let g = ground ls in zip3 (g : ls) ls (drop 1 ls ++ [g])
    ground (h:_) = length h `replicate` '.'
    enum = zip [0..]
    neighbours ck = foldl f (Nothing, M.empty) (enum ck) where
      f m (y, (p, c, n)) = foldl g m (enum c) where
          g r     (x, '.') = r
          g (_, m) (x, 'S') = let k = (x, y) in (Just k, m)
          g (s, m) (x,  i ) = let k = (x, y) in (s, M.insert k (neighbour i k) m)
    neighbour '|' p = (north p, south p)
    neighbour '-' p = (east p, west p)
    neighbour 'L' p = (north p, east p)
    neighbour 'J' p = (north p, west p)
    neighbour '7' p = (south p, west p)
    neighbour 'F' p = (south p, east p)
    north (x,y) = (x,y-1)
    south (x,y) = (x,y+1)
    east (x,y) = (x-1,y)
    west (x,y) = (x+1,y)
    fixStart (Nothing, _) = error "input does not contain a start node"
    fixStart (Just s, m) = (s, m) -- TODO

p1 = id
