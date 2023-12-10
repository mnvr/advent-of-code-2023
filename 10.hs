import Data.Map qualified as M

main :: IO ()
main = interact $ (++ "\n") . show . p1 . lines

p1 = neighbours . chunks where
  chunks ls = let g = ground ls in zip3 (g : ls) ls (drop 1 ls ++ [g])
  ground (h:_) = length h `replicate` '.'
  enum = zip [0..]
  neighbours ck = foldl f M.empty (enum ck) where
    f m (y, (p,c,n)) = foldl g m (enum c) where
        g m (x, s) = let pos = (x, y) in M.insert pos (neighbour s pos) m
  neighbour '|' (x, y) = [(x,y-1), (x,y+1)]
  neighbour _ (x, y) = [(x,y-1), (x,y+1)]
