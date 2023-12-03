main :: IO ()
main = interact $ (++"\n") . show . parse

parse = chunks

-- Overlapping chunks / neighbourhoods of 3
chunks :: String -> [(String, String, String)]
chunks s = zip3 (blank : ls) ls (tail ls ++ [blank])
  where ls = take 4 . lines $ s
        blank = replicate (length $ head ls) '.'
