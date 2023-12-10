main :: IO ()
main = interact $ (++ "\n") . show . p1 . lines

p1 ls = zip3 ls (drop 1 ls) (drop 2 ls)
