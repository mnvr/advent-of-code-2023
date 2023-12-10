import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

parse = neighbours . chunks . lines
  where
    chunks ls = let g = ground ls in zip3 (g : ls) ls (drop 1 ls ++ [g])
    ground (h:_) = length h `replicate` '.'
    enum = zip [0..]
    neighbours ck = foldl f M.empty (enum ck) where
      f m (y, (p,c,n)) = foldl g m (enum c) where
          g m (x, s) = let pos = (x, y) in M.alter (\_-> neighbour s pos) pos m
    neighbour '|' p = Just [north p, south p]
    neighbour '-' p = Just [east p, west p]
    neighbour 'L' p = Just [north p, east p]
    neighbour 'J' p = Just [north p, west p]
    neighbour '7' p = Just [south p, west p]
    neighbour 'F' p = Just [south p, east p]
    neighbour '.' _ = Nothing
    neighbour 'S' p = Just [p] -- We'll connect this in the next pass
    north (x,y) = (x,y-1)
    south (x,y) = (x,y+1)
    east (x,y) = (x-1,y)
    west (x,y) = (x+1,y)

p1 = id
