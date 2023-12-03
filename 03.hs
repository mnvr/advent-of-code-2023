main :: IO ()
main = interact $ (++"\n") . show . parse

parse s = chunks
  where chunks = zipWith3 zChars (blank : ls) ls (tail ls ++ [blank])
        ls = take 4 . lines $ s
        blank = replicate (length $ head ls) '.'
        zChars = zipWith3 (\c1 c2 c3 -> [c1, c2, c3])
