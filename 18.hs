main :: IO ()
main = interact $ (++ "\n") . show . p1 . parse

type Step = (Char, Int)

parse :: String -> [Step]
parse = map (line . words) . lines
  where line ([(d:_),c,_]) = (d, (read c :: Int))

p1 = id
