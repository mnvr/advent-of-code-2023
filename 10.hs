main :: IO ()
main = interact $ (++ "\n") . show . p1 . lines

p1 ls = let g = ground ls in zip3 (g : ls) ls (drop 1 ls ++ [g])

ground (h:_) = length h `replicate` '.'
